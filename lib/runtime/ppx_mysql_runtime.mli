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
  PPX_MYSQL_CONTEXT with type 'a IO.t = 'a M.IO.t and type Prepared.dbh = M.Prepared.dbh
