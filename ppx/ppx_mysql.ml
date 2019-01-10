open Ppxlib
open Ppx_mysql_runtime.Stdlib

(* So the unit tests have access to the Query module *)
module Query = Query
module Buildef = Ast_builder.Default

(* [split_n] has the same signature and semantics as its homonym in Base.
 * [split_n xs n] is [(take xs n, drop xs n)].
 *)
let split_n elems index =
  let rec loop accum leftovers index =
    match leftovers, index with
    | _, x
      when x <= 0 ->
        List.rev accum, leftovers
    | [], _ ->
        List.rev accum, leftovers
    | hd :: tl, i ->
        loop (hd :: accum) tl (i - 1)
  in
  loop [] elems index

let create_unique_var ~loc params base =
  let already_exists name =
    List.exists (fun param -> Query.(param.name) = name) params
  in
  let rec add_suffix counter =
    let candidate = Printf.sprintf "%s_%d" base counter in
    match already_exists candidate with
    | true ->
        add_suffix (counter + 1)
    | false ->
        candidate
  in
  let name =
    match already_exists base with
    | true ->
        add_suffix 0
    | false ->
        base
  in
  let pat = Buildef.ppat_var ~loc (Loc.make ~loc name) in
  let ident = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident name)) in
  pat, ident

let rec build_fun_chain ~loc expr = function
  | [] ->
      expr
  | Query.({typ; opt; name; _}) :: tl ->
      let open Buildef in
      let tl' = build_fun_chain ~loc expr tl in
      let var = ppat_var ~loc (Loc.make ~loc name) in
      let basetyp = ptyp_constr ~loc (Loc.make ~loc (Lident typ)) [] in
      let fulltyp =
        match opt with
        | true ->
            ptyp_constr ~loc (Loc.make ~loc (Lident "option")) [basetyp]
        | false ->
            basetyp
      in
      let pat = ppat_constraint ~loc var fulltyp in
      pexp_fun ~loc (Labelled name) None pat tl'

let build_in_param ~loc param =
  let to_string_mod, to_string_fun = Query.(param.to_string) in
  let to_string =
    Buildef.pexp_ident ~loc (Loc.make ~loc (Ldot (Lident to_string_mod, to_string_fun)))
  in
  let arg = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.name)) in
  match param.opt with
  | true ->
      [%expr (Ppx_mysql_runtime.Stdlib.Option.map [%e to_string]) [%e arg]]
  | false ->
      [%expr Ppx_mysql_runtime.Stdlib.Option.Some ([%e to_string] [%e arg])]

let build_out_param_processor ~loc out_params =
  let make_elem i param =
    let of_string_mod, of_string_fun = Query.(param.of_string) in
    let of_string =
      Buildef.pexp_ident
        ~loc
        (Loc.make ~loc (Ldot (Lident of_string_mod, of_string_fun)))
    in
    let param_name = Buildef.estring ~loc Query.(param.name) in
    let of_string_desc =
      Buildef.estring ~loc @@ Printf.sprintf "%s.%s" of_string_mod of_string_fun
    in
    let arg = [%expr Ppx_mysql_runtime.Stdlib.Array.get row [%e Buildef.eint ~loc i]] in
    let appl =
      [%expr
        let deserialize value =
          try [%e of_string] value with Failure _ ->
            raise (Deserialization_error ([%e param_name], [%e of_string_desc], value))
        in
        Ppx_mysql_runtime.Stdlib.Option.map deserialize [%e arg]]
    in
    match param.opt with
    | true ->
        appl
    | false ->
        [%expr
          try Ppx_mysql_runtime.Stdlib.Option.get [%e appl] with Invalid_argument _ ->
            raise (Expected_non_null_column [%e param_name])]
  in
  let ret_expr =
    match out_params with
    | [] ->
        [%expr ()]
    | [x] ->
        make_elem 0 x
    | _ :: _ :: _ ->
        Buildef.pexp_tuple ~loc @@ List.mapi make_elem out_params
  in
  let len_expected = Buildef.eint ~loc (List.length out_params) in
  [%expr
    fun row ->
      (let exception Deserialization_error of string * string * string in
      (let exception Expected_non_null_column of string in
      let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
      let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
      if len_row = [%e len_expected]
      then
        try Ppx_mysql_runtime.Stdlib.Result.Ok [%e ret_expr] with
        | Deserialization_error (col, f, v) ->
            Ppx_mysql_runtime.Stdlib.Result.Error
              (`Column_errors [col, `Deserialization_error (f, v)])
        | Expected_non_null_column col ->
            Ppx_mysql_runtime.Stdlib.Result.Error
              (`Column_errors [col, `Expected_non_null_value])
      else
        Ppx_mysql_runtime.Stdlib.Result.Error
          (`Unexpected_number_of_columns (len_row, [%e len_expected]))) [@warning "-38"]) 
      [@warning "-38"]]

let build_process_rows ~loc = function
  | "select_one" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop [row']
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | [], Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
              | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
              | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
            in
            loop []]
  | "select_opt" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop [row']
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | [], Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       Ppx_mysql_runtime.Stdlib.Option.None)
              | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_maybe_one_found_many)
              | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       (Ppx_mysql_runtime.Stdlib.Option.Some hd))
            in
            loop []]
  | "select_all" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= function
              | Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop (row' :: acc)
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       (Ppx_mysql_runtime.Stdlib.List.rev acc))
            in
            loop []]
  | "execute" ->
      Ok
        [%expr
          fun () ->
            Prepared.fetch stmt_result
            >>= function
            | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                IO.return
                  (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
            | Ppx_mysql_runtime.Stdlib.Option.None ->
                IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ())]
  | etc ->
      Error (`Unknown_query_variant etc)

let actually_expand ~loc sql_variant query =
  let open Result in
  build_process_rows ~loc sql_variant
  >>= fun process_rows ->
  Query.parse query
  >>= fun {sql; in_params; out_params; list_params} ->
  Query.remove_duplicates in_params
  >>= fun unique_in_params ->
  let dbh_pat, dbh_ident = create_unique_var ~loc unique_in_params "dbh" in
  let elems_pat, elems_ident = create_unique_var ~loc unique_in_params "elems" in
  ( match list_params with
  | None ->
      let sql_expr = Buildef.estring ~loc sql in
      let param_expr =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) in_params
      in
      Ok
        [%expr
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ([%e sql_expr], [%e param_expr]))]
  | Some {subsql; string_index; param_index; params} ->
      Query.remove_duplicates params
      >>= fun unique_params ->
      let subsql_expr = Buildef.estring ~loc subsql in
      let sql_before = Buildef.estring ~loc @@ String.sub sql 0 string_index in
      let sql_after =
        Buildef.estring ~loc
        @@ String.sub sql string_index (String.length sql - string_index)
      in
      let params_before, params_after = split_n in_params param_index in
      let params_before =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) params_before
      in
      let params_after =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) params_after
      in
      let list_params_decl =
        let make_elem param = Buildef.ppat_var ~loc (Loc.make ~loc Query.(param.name)) in
        Buildef.ppat_tuple ~loc @@ List.map make_elem unique_params
      in
      let list_params_conv =
        Buildef.elist ~loc @@ List.map (build_in_param ~loc) params
      in
      Ok
        [%expr
          match [%e elems_ident] with
          | [] ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Empty_input_list)
          | elems ->
              let subsqls =
                Ppx_mysql_runtime.Stdlib.List.map (fun _ -> [%e subsql_expr]) elems
              in
              let patch = Ppx_mysql_runtime.Stdlib.String.concat ", " subsqls in
              let sql =
                Ppx_mysql_runtime.Stdlib.String.append
                  [%e sql_before]
                  (Ppx_mysql_runtime.Stdlib.String.append patch [%e sql_after])
              in
              let params_between =
                Array.of_list
                  (List.concat
                     (List.map (fun [%p list_params_decl] -> [%e list_params_conv]) elems))
              in
              let params =
                Ppx_mysql_runtime.Stdlib.Array.concat
                  [[%e params_before]; params_between; [%e params_after]]
              in
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok (sql, params))] )
  >>= fun setup_expr ->
  (* Note that in the expr fragment below we disable warning 26 (about unused variables)
     for the 'process_out_params' function, since it may indeed be unused if there are
     no output parameters. *)
  let expr =
    [%expr
      let open IO_result in
      [%e setup_expr]
      >>= fun (sql, params) ->
      let[@warning "-26"] process_out_params =
        [%e build_out_param_processor ~loc out_params]
      in
      Prepared.with_stmt [%e dbh_ident] sql (fun stmt ->
          Prepared.execute_null stmt params >>= fun stmt_result -> [%e process_rows] ()
      )]
  in
  let chain = build_fun_chain ~loc expr unique_in_params in
  let chain =
    match list_params with
    | None ->
        chain
    | Some _ ->
        Buildef.pexp_fun ~loc Nolabel None elems_pat chain
  in
  Ok (Buildef.pexp_fun ~loc Nolabel None dbh_pat chain)

let expand ~loc ~path:_ sql_variant query =
  match actually_expand ~loc sql_variant query with
  | Ok expr ->
      expr
  | Error err ->
      let msg =
        match err with
        | #Query.error as err ->
            Query.explain_error err
        | `Unknown_query_variant variant ->
            Printf.sprintf "I don't understand query variant '%s'" variant
      in
      raise
        (Location.Error
           (Location.Error.createf ~loc "Error in 'mysql' extension: %s" msg))

let pattern =
  Ast_pattern.(pexp_apply (pexp_ident (lident __)) (pair nolabel (estring __) ^:: nil))

let name = "mysql"

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ext]
