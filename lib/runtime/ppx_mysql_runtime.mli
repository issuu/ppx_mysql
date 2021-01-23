type deserialization_error =
  { idx : int
  ; name : string
  ; func : string
  ; value : string
  ; message : string
  }

type column_error =
  [ `Expected_non_null_column of int * string
  | `Deserialization_error of deserialization_error
  ]

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
    type error

    val create : dbh -> string -> (stmt, error) result IO.t
    val close : stmt -> (unit, error) result IO.t
    val execute_null : stmt -> string option array -> (stmt_result, error) result IO.t
    val fetch : stmt_result -> (string option array option, error) result IO.t
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
    type ('a, 'e) t = ('a, 'e) result IO.t

    val return : 'a -> ('a, 'e) t
    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module Prepared : sig
    type dbh
    type stmt
    type stmt_result
    type error
    type wrapped_dbh
    type wrapped_error = [ `Mysql_error of error ]

    val init : dbh -> wrapped_dbh

    val execute_null
      :  stmt
      -> string option array
      -> (stmt_result, [> wrapped_error ]) result IO.t

    val fetch
      :  stmt_result
      -> (string option array option, [> wrapped_error ]) result IO.t

    val with_stmt_cached
      :  wrapped_dbh
      -> string
      -> (stmt -> ('a, ([> wrapped_error ] as 'e)) result IO.t)
      -> ('a, 'e) result IO.t

    val with_stmt_uncached
      :  wrapped_dbh
      -> string
      -> (stmt -> ('a, ([> wrapped_error ] as 'e)) result IO.t)
      -> ('a, 'e) result IO.t
  end
end

module Make_context (M : PPX_MYSQL_CONTEXT_ARG) :
  PPX_MYSQL_CONTEXT
    with type 'a IO.t = 'a M.IO.t
     and type Prepared.dbh = M.Prepared.dbh
     and type Prepared.error = M.Prepared.error

module Stdlib : sig
  module Array : sig
    include module type of struct
      include Array
    end
  end

  module List : sig
    include module type of struct
      include List
    end
  end

  module Option : sig
    type 'a t = 'a option =
      | None
      | Some of 'a

    val map : ('a -> 'b) -> 'a t -> 'b t
    val get : 'a t -> 'a
  end

  module Result : sig
    type ('a, 'e) t = ('a, 'e) result =
      | Ok of 'a
      | Error of 'e

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module String : sig
    include module type of struct
      include String
    end

    val append : string -> string -> string
  end

  val ( = ) : 'a -> 'a -> bool
end
