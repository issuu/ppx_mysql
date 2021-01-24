open Base
open Async

type deserialization_error =
  { idx : int
  ; name : string
  ; func : string
  ; value : string
  ; message : string
  }
[@@deriving sexp_of]

type column_error =
  [ `Expected_non_null_column of int * string
  | `Deserialization_error of deserialization_error
  ]
[@@deriving sexp_of]

let sexp_of_column_errors errs =
  [%sexp_of: [ `Column_errors of column_error list ]] (`Column_errors errs)
;;

let sexp_of_unexpected_number_of_columns (len, len_expected) =
  [%sexp_of: [ `Unexpected_number_of_columns of int * int ]]
    (`Unexpected_number_of_columns (len, len_expected))
;;

type 'a deserializer = string -> 'a

let wrap_failure : (string -> 'a) -> 'a deserializer =
 fun of_string s ->
  match of_string s with
  | v -> v
  | exception Failure _ -> failwith "cannot parse number"
;;

let string_of_string str = str
let int_of_string = wrap_failure Stdlib.int_of_string
let int32_of_string = wrap_failure Int32.of_string
let int64_of_string = wrap_failure Int64.of_string

let bool_of_string str =
  match Stdlib.int_of_string str with
  | v -> v <> 0
  | exception Failure _ -> failwith "cannot parse boolean"
;;

external identity : 'a -> 'a = "%identity"

let deserialize_non_nullable_column idx name of_string func err_accum = function
  | None ->
    let err = `Expected_non_null_column (idx, name) in
    None, err :: err_accum
  | Some value ->
    (match of_string value with
    | v -> Some v, err_accum
    | exception e ->
      let err =
        `Deserialization_error { idx; name; func; value; message = Exn.to_string e }
      in
      None, err :: err_accum)
;;

let deserialize_nullable_column idx name of_string func err_accum = function
  | None -> Some None, err_accum
  | Some value ->
    (match of_string value with
    | v -> Some (Some v), err_accum
    | exception e ->
      let err =
        `Deserialization_error { idx; name; func; value; message = Exn.to_string e }
      in
      None, err :: err_accum)
;;

module type SERIALIZABLE = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module Prepared = struct
  type wrapped_dbh =
    { dbh : Mysql.dbd
    ; stmt_cache : (string, Mysql.Prepared.stmt) Hashtbl.t
    }

  let init dbh = { dbh; stmt_cache = Hashtbl.create (module String) ~size:16 }

  let create dbh sql =
    Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.create dbh sql))
  ;;

  let create_or_reuse { dbh; stmt_cache } sql =
    match Hashtbl.find stmt_cache sql with
    | Some stmt -> Deferred.Or_error.return stmt
    | None ->
      let%bind.Deferred.Or_error stmt = create dbh sql in
      Hashtbl.set stmt_cache ~key:sql ~data:stmt;
      Deferred.Or_error.return stmt
  ;;

  let close stmt =
    Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.close stmt))
  ;;

  let execute_null stmt args =
    Deferred.(
      Or_error.try_with (fun () -> return @@ (Mysql.Prepared.execute_null stmt) args))
  ;;

  let fetch stmt_res =
    Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.fetch stmt_res))
  ;;

  let with_stmt_cached wrapped_dbh sql f =
    Deferred.Or_error.bind (create_or_reuse wrapped_dbh sql) ~f:(fun stmt -> f stmt)
  ;;

  let with_stmt_uncached { dbh; stmt_cache = _ } sql f =
    let%bind.Deferred.Or_error stmt = create dbh sql in
    let%bind res = f stmt in
    match%bind close stmt with
    | Ok () -> return res
    | Error _ as e -> return e
  ;;
end
