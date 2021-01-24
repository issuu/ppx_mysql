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

val sexp_of_column_errors : column_error list -> Sexp.t
val sexp_of_unexpected_number_of_columns : int * int -> Sexp.t

type 'a deserializer = string -> 'a

val string_of_string : string deserializer
val int_of_string : int deserializer
val int32_of_string : int32 deserializer
val int64_of_string : int64 deserializer
val bool_of_string : bool deserializer
external identity : 'a -> 'a = "%identity"

val deserialize_non_nullable_column
  :  int
  -> string
  -> 'a deserializer
  -> string
  -> column_error list
  -> string option
  -> 'a option * column_error list

val deserialize_nullable_column
  :  int
  -> string
  -> 'a deserializer
  -> string
  -> column_error list
  -> string option
  -> 'a option option * column_error list

module type SERIALIZABLE = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module Prepared : sig
  type wrapped_dbh

  val init : Mysql.dbd -> wrapped_dbh

  val execute_null
    :  Mysql.Prepared.stmt
    -> string option array
    -> Mysql.Prepared.stmt_result Deferred.Or_error.t

  val fetch : Mysql.Prepared.stmt_result -> string option array option Deferred.Or_error.t

  val with_stmt_cached
    :  wrapped_dbh
    -> string
    -> (Mysql.Prepared.stmt -> 'a Deferred.Or_error.t)
    -> 'a Deferred.Or_error.t

  val with_stmt_uncached
    :  wrapped_dbh
    -> string
    -> (Mysql.Prepared.stmt -> 'a Deferred.Or_error.t)
    -> 'a Deferred.Or_error.t
end
