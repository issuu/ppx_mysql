(* This module is required to keep references to the OCaml operators in the stdlib *)
module Stdlib = struct
  module Array = Array
  module List = List

  module Option = struct
    type 'a t = 'a option =
      | None
      | Some of 'a

    let map f = function
      | Some x ->
          Some (f x)
      | None ->
          None

    let get = function
      | Some x ->
          x
      | None ->
          invalid_arg "Option.get"
  end

  module Result = struct
    type ('a, 'e) t = ('a, 'e) result =
      | Ok of 'a
      | Error of 'e

    let bind r f =
      match r with
      | Ok x ->
          f x
      | Error _ as e ->
          e

    let ( >>= ) = bind
  end

  let ( = ) = ( = )
end

exception Deserialization_error of string * string

let wrap_deserializer f x =
  try f x
  with Failure msg -> raise (Deserialization_error (msg, x))

let identity x = x

let int_of_string_exn = wrap_deserializer int_of_string
let int32_of_string_exn = wrap_deserializer Int32.of_string
let int64_of_string_exn = wrap_deserializer Int64.of_string
