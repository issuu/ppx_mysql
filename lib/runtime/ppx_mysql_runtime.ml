open Base

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

module type PPX_MYSQL_CONTEXT_ARG = sig
  module IO : sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Prepared : sig
    type dbh
    type stmt
    type stmt_result

    val create : dbh -> string -> stmt Base.Or_error.t IO.t
    val close : stmt -> unit Base.Or_error.t IO.t
    val execute_null : stmt -> string option array -> stmt_result Base.Or_error.t IO.t
    val fetch : stmt_result -> string option array option Base.Or_error.t IO.t
  end
end

module type PPX_MYSQL_CONTEXT = sig
  module IO : sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module IO_result : sig
    type ('a, 'e) t = ('a, 'e) Result.t IO.t

    val return : 'a -> ('a, 'e) t
    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module Prepared : sig
    type dbh
    type stmt
    type stmt_result
    type wrapped_dbh

    val init : dbh -> wrapped_dbh
    val execute_null : stmt -> string option array -> stmt_result Base.Or_error.t IO.t
    val fetch : stmt_result -> string option array option Base.Or_error.t IO.t

    val with_stmt_cached
      :  wrapped_dbh
      -> string
      -> (stmt -> 'a Base.Or_error.t IO.t)
      -> 'a Base.Or_error.t IO.t

    val with_stmt_uncached
      :  wrapped_dbh
      -> string
      -> (stmt -> 'a Base.Or_error.t IO.t)
      -> 'a Base.Or_error.t IO.t
  end
end

module Make_context (M : PPX_MYSQL_CONTEXT_ARG) :
  PPX_MYSQL_CONTEXT with type 'a IO.t = 'a M.IO.t and type Prepared.dbh = M.Prepared.dbh =
struct
  module IO = struct
    include M.IO

    let ( >>= ) = bind
  end

  module IO_result = struct
    type ('a, 'e) t = ('a, 'e) Result.t IO.t

    let return x = IO.return @@ Ok x

    let bind x f =
      IO.bind x (function
          | Ok v -> f v
          | Error _ as e -> IO.return e)
    ;;

    let ( >>= ) = bind
  end

  module Prepared = struct
    type dbh = M.Prepared.dbh
    type stmt = M.Prepared.stmt
    type stmt_result = M.Prepared.stmt_result

    type wrapped_dbh =
      { dbh : dbh
      ; stmt_cache : (string, stmt) Hashtbl.t
      }

    let init dbh = { dbh; stmt_cache = Hashtbl.create (module String) ~size:16 }
    let create dbh sql = (M.Prepared.create dbh) sql

    let create_or_reuse { dbh; stmt_cache } sql =
      match Hashtbl.find stmt_cache sql with
      | Some stmt -> IO_result.return stmt
      | None ->
        IO_result.bind (create dbh sql)
        @@ fun stmt ->
        Hashtbl.set stmt_cache ~key:sql ~data:stmt;
        IO_result.return stmt
    ;;

    let close stmt = M.Prepared.close stmt
    let execute_null stmt args = (M.Prepared.execute_null stmt) args
    let fetch stmt_res = M.Prepared.fetch stmt_res

    let with_stmt_cached wrapped_dbh sql f =
      IO_result.bind (create_or_reuse wrapped_dbh sql) @@ fun stmt -> f stmt
    ;;

    let with_stmt_uncached { dbh; stmt_cache = _ } sql f =
      IO_result.bind (create dbh sql)
      @@ fun stmt ->
      IO.bind (f stmt)
      @@ fun res ->
      IO.bind (close stmt)
      @@ function
      | Ok () -> IO.return res
      | Error _ as e -> IO.return e
    ;;
  end
end

(* module Stdlib = struct *)

(*   module Array = Array *)
(*   module List = List *)

(*   module Option = struct *)
(*     type 'a t = 'a option = *)
(*       | None *)
(*       | Some of 'a *)

(*     let map f = function *)
(*       | Some x -> Some (f x) *)
(*       | None -> None *)
(*     ;; *)

(*     let get = function *)
(*       | Some x -> x *)
(*       | None -> invalid_arg "Option.get" *)
(*     ;; *)
(*   end *)

(*   module Result = struct *)
(*     type ('a, 'e) t = ('a, 'e) result = *)
(*       | Ok of 'a *)
(*       | Error of 'e *)

(*     let bind r f = *)
(*       match r with *)
(*       | Ok x -> f x *)
(*       | Error _ as e -> e *)
(*     ;; *)

(*     let ( >>= ) = bind *)
(*   end *)

(*   module String = struct *)
(*     include String *)

(*     let append = ( ^ ) *)
(*   end *)

(*   let ( = ) = ( = ) *)
(* end *)
