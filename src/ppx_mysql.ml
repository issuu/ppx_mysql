(********************************************************************************)
(* Ppx_mysql.ml                                                                 *)
(********************************************************************************)

open Ppxlib

module Used_set = Set.Make (String)
module Buildef = Ast_builder.Default


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type param =
    {
    typ: string;
    opt: bool;
    name: string;
    of_string: string;
    to_string: string;
    }

type parsed_query =
    {
    query: string;
    in_params: param list;
    out_params: param list;
    }

type parse_error =
    [ `Bad_param of string
    | `Escape_at_end
    | `Unknown_mysql_type of string
    | `Unterminated_string
    ]


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

(* FIXME: 'stringly'-typed... *)
let ocaml_of_mysql = function
    | "INT"  -> Ok ("int64", "Int64.of_string", "Int64.to_string")
    | "TEXT" -> Ok ("string", "Ppx_mysql_runtime.identity", "Ppx_mysql_runtime.identity")
    | _      -> Error ()

let parse_query =
    let param_re = Re.(seq [
        group (rep1 (compl [char '{'; char '?']));
        group (opt (char '?'));
        char '{';
        group (rep1 (compl [char '}']));
        char '}';
        ]) |> Re.compile in
    fun query ->
        let len = String.length query in
        let buf = Buffer.create len in
        let rec main_loop i string_delim acc_in acc_out =
            if i >= len
            then match string_delim with
                | None   -> Ok {query = Buffer.contents buf; in_params = List.rev acc_in; out_params = List.rev acc_out}
                | Some _ -> Error `Unterminated_string
            else
                let this = query.[i] in
                match string_delim with
                    | _ when this = '\\' ->
                        Buffer.add_char buf this;
                        if i + 1 >= len
                        then
                            Error `Escape_at_end
                        else begin
                            Buffer.add_char buf query.[i + 1];
                            main_loop (i + 2) string_delim acc_in acc_out
                        end
                    | None when this = '\'' || this = '"' ->
                        Buffer.add_char buf this;
                        main_loop (i + 1) (Some this) acc_in acc_out
                    | None when this = '%' ->
                        parse_param (i + 1) `In_param acc_in acc_out
                    | None when this = '@' ->
                        parse_param (i + 1) `Out_param acc_in acc_out
                    | Some delim when this = delim ->
                        Buffer.add_char buf this;
                        if i + 1 < len && query.[i + 1] = delim
                        then begin
                            Buffer.add_char buf this;
                            main_loop (i + 2) string_delim acc_in acc_out
                        end
                        else begin
                            main_loop (i + 1) None acc_in acc_out
                        end
                    | _ ->
                        Buffer.add_char buf this;
                        main_loop (i + 1) string_delim acc_in acc_out
        and parse_param i param_typ acc_in acc_out =
            match Re.exec_opt ~pos:i param_re query with
                | None ->
                    let until = match String.index_from_opt query (i - 1) ' ' with
                        | Some x -> x
                        | None -> String.length query in
                    Error (`Bad_param (String.sub query (i - 1) (until - i + 1)))
                | Some groups ->
                    begin match Re.Group.all groups with
                        | [| all; typ; opt; name |] ->
                            begin match ocaml_of_mysql typ with
                                | Ok (typ, of_string, to_string) ->
                                    let param = {typ; opt = opt = "?"; name; of_string; to_string} in
                                    let (replacement, acc_in, acc_out) = match param_typ with
                                        | `In_param  -> ("?", param :: acc_in, acc_out)
                                        | `Out_param -> (name, acc_in, param :: acc_out) in
                                    Buffer.add_string buf replacement;
                                    main_loop (i + String.length all) None acc_in acc_out
                                | Error () ->
                                    Error (`Unknown_mysql_type typ)
                            end
                        | _ ->
                            assert false (* This should never happen. *)
                    end
        in main_loop 0 None [] []

let explain_parse_error = function
    | `Bad_param str          -> Printf.sprintf "Syntax error on parameter specification '%s'" str
    | `Escape_at_end          -> "The last character of the query cannot be an escape character"
    | `Unknown_mysql_type typ -> Printf.sprintf "Unknown MySQL type '%s'" typ
    | `Unterminated_string    -> "The query contains an unterminated string"
    
let rec build_fun_chain ~loc expr used_set = function
    | [] ->
        expr
    | {name; _} :: tl when Used_set.mem name used_set ->
        build_fun_chain ~loc expr used_set tl
    | {typ; opt; name; _} :: tl ->
        let open Buildef in
        let used_set = Used_set.add name used_set in
        let tl' = build_fun_chain ~loc expr used_set tl in
        let var = ppat_var ~loc (Loc.make ~loc name) in
        let basetyp = ptyp_constr ~loc (Loc.make ~loc (Lident typ)) [] in
        let fulltyp =
            if opt
            then ptyp_constr ~loc (Loc.make ~loc (Lident "option")) [basetyp]
            else basetyp in
        let pat = ppat_constraint ~loc var fulltyp in
        pexp_fun ~loc (Labelled name) None pat tl'

let build_in_param ~loc param =
    let f = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.to_string)) in
    let arg = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.name)) in
    if param.opt
    then [%expr (Ppx_mysql_runtime.map_option [%e f]) [%e arg]]
    else [%expr Some ([%e f] [%e arg])]

let build_out_param_processor ~loc out_params =
    let make_elem i param =
        let f = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.of_string)) in
        let arg = [%expr row.([%e Buildef.eint ~loc i])] in
        let appl = [%expr (Ppx_mysql_runtime.map_option [%e f]) [%e arg]] in
        if param.opt
        then appl
        else [%expr (Ppx_mysql_runtime.get_option [%e appl])] in
    let ret_expr = match out_params with
        | []     -> [%expr ()]
        | [x]    -> make_elem 0 x
        | _ :: _ -> Buildef.pexp_tuple ~loc (List.mapi make_elem out_params) in
    [%expr fun row ->
        if Array.length row = [%e Buildef.eint ~loc (List.length out_params)]
        then [%e ret_expr]
        else assert false (* FIXME *)
        ]

let expand ~loc ~path:_ (sql_variant: string) (query: string) =
    let postproc = match sql_variant with
        | "Select_one" -> "select_one"
        | "Select_opt" -> "select_opt"
        | "Select_all" -> "select_all"
        | "Execute"    -> "execute"
        | _            -> assert false in (* FIXME *)
    let fq_postproc = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident ("Ppx_mysql_runtime." ^ postproc))) in
    match parse_query query with
        | Ok {query; in_params; out_params} ->
            let expr =
                [%expr
                let open Ppx_mysql_aux.IO in
                let query = [%e Buildef.estring ~loc query] in
                let params = [%e Buildef.(pexp_array ~loc @@ List.map (build_in_param ~loc) in_params) ] in
                let process_out_params = [%e build_out_param_processor ~loc out_params] in
                Ppx_mysql_aux.Prepared.with' dbh query @@ fun stmt ->
                    Ppx_mysql_aux.Prepared.execute stmt params >>= fun stmt_result ->
                    Ppx_mysql_aux.Prepared.map process_out_params stmt_result >>= fun xs ->
                    Ppx_mysql_aux.IO.return ([%e fq_postproc] xs)
                ] in
            let dbh_pat = Buildef.ppat_var ~loc (Loc.make ~loc "dbh") in
            let chain = build_fun_chain ~loc expr Used_set.empty in_params in
            Buildef.pexp_fun ~loc Nolabel None dbh_pat chain
        | Error err ->
            let msg = explain_parse_error err in
            raise (Location.Error (Location.Error.createf ~loc "Error in 'mysql' extension: %s" msg))

let pattern =
    Ast_pattern.(pexp_construct (lident __) (some (estring __)))

let name = "mysql"

let ext = Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () =
    Driver.register_transformation name ~extensions:[ext]