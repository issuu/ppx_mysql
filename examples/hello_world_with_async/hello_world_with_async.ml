(* This example assumes that a Mysql database 'test' exists for user 'root'.
 * Moreover, a table 'users' defined as follows is also present in the DB:
 *  
 * CREATE TABLE users
 *     (
 *     id    INT NOT NULL,
 *     name  TEXT NOT NULL,
 *     phone TEXT NULL,
 *     PRIMARY KEY (id)
 *     );
 *)

open Core
open Async

let stdout = Lazy.force Writer.stdout

(** Module implementing custom (de)serialization to/from MySQL. *)

module Phone : Ppx_mysql_runtime.SERIALIZABLE with type t = string = struct
  type t = string

  let of_string str = if String.length str <= 9 then str else failwith "string too long"
  let to_string str = str
end

(** The user type used throughout this example. *)

type user =
  { id : int32
  ; name : string
  ; phone : Phone.t option
  }

let user_of_tuple (id, name, phone) = { id; name; phone }

let print_user { id; name; phone } =
  Writer.writef
    stdout
    "\t%ld -> %s (phone: %s)\n"
    id
    name
    (match phone with
    | Some p -> p
    | None -> "--")
;;

(** Database queries using the Ppx_mysql syntax extension. *)

let get_all_users dbh =
  let open Deferred.Result in
  [%mysql select_all "SELECT @int32{id}, @string{name}, @string?{phone} FROM users"] dbh
  >>| List.map ~f:user_of_tuple
;;

let get_some_users dbh ids =
  let open Deferred.Result in
  [%mysql
    select_all
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id IN \
       (%list{%int32{id}})"]
    dbh
    ids
  >>| List.map ~f:user_of_tuple
;;

;;
fun dbh elems ->
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "?") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "SELECT id, name, phone FROM users WHERE id IN (" ^ patch ^ ")"
    in
    let params_between =
      Array.of_list
        (Base.List.concat
           (Base.List.map ~f:(fun id -> [ Base.Option.Some (Int32.to_string id) ]) elems))
    in
    let params = Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 3
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int32_of_string
          "Ppx_mysql_runtime.int32_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      let col2, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          2
          "phone"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(2)
      in
      match col0, col1, col2 with
      | Base.Option.Some v0, Base.Option.Some v1, Base.Option.Some v2 ->
        Result.Ok (v0, v1, v2)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 3)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= function
          | Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> Async.return err)
          | None -> Async.return (Result.Ok (Base.List.rev acc))
        in
        loop [])
        ())

let get_user dbh ~id =
  let open Deferred.Result in
  [%mysql
    select_one
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id = %int32{id}"]
    dbh
    ~id
  >>| user_of_tuple
;;

let get_user_opt dbh ~id =
  let open Deferred.Result in
  let f =
    [%mysql
      select_one
        "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id = \
         %int32?{id}"]
  in
  f dbh ~id >>| user_of_tuple
;;

let insert_user =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES (%int32{id}, %string{name}, \
       %string?{phone})"]
;;

let insert_users =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES %list{(%int32{id}, %string{name}, \
       %string?{phone})}"]
;;

let update_user =
  [%mysql
    execute
      "UPDATE users SET name = %string{name}, phone = %string?{phone} WHERE id = \
       %int32{id}"]
;;

let delete_user = [%mysql execute "DELETE FROM users WHERE id = %int32{id}"]

(** Main functions and values. *)

let test dbh =
  let open Deferred.Result in
  insert_user dbh ~id:1l ~name:"John" ~phone:(Some "123456")
  >>= fun () ->
  insert_user dbh ~id:2l ~name:"Jane" ~phone:None
  >>= fun () ->
  insert_user dbh ~id:3l ~name:"Claire" ~phone:None
  >>= fun () ->
  insert_users dbh [ 4l, "Mark", None; 5l, "Alice", Some "234567" ]
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Writer.writef stdout "All users:\n";
  List.iter ~f:print_user users;
  get_some_users dbh [ 1l; 2l; 3l ]
  >>= fun users ->
  Writer.writef stdout "Users with ID in {1, 2, 3}:\n";
  List.iter ~f:print_user users;
  update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321")
  >>= fun () ->
  get_user dbh ~id:2l
  >>= fun user ->
  Writer.writef stdout "User with ID = 2 after update:\n";
  print_user user;
  delete_user dbh ~id:3l
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Writer.writef stdout "All users after deleting one with ID = 3:\n";
  List.iter ~f:print_user users;
  return ()
;;

let main () =
  let open Deferred.Infix in
  let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
  let wrapped_dbh = Ppx_mysql_runtime.Prepared.init dbh in
  test wrapped_dbh
  >>= fun res ->
  Mysql.disconnect dbh;
  match res with
  | Ok () ->
    Writer.writef stdout "All went well!\n";
    return ()
  | Error _ ->
    Writer.writef stdout "An error occurred!\n";
    return ()
;;

let () = Command.(run @@ async ~summary:"Run Async example" @@ Param.return main)
