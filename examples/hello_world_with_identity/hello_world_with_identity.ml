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

open Mysql_with_identity

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
  Printf.printf
    "\t%ld -> %s (phone: %s)\n"
    id
    name
    (match phone with
    | Some p -> p
    | None -> "--")
;;

(** Database queries using the Ppx_mysql syntax extension. *)

let ( >>| ) x f =
  let open IO_result in
  x >>= fun x' -> return @@ f x'
;;

let get_all_users dbh =
  [%mysql select_all "SELECT @int32{id}, @string{name}, @Phone?{phone} FROM users"] dbh
  >>| List.map user_of_tuple
;;

let get_some_users dbh ids =
  [%mysql
    select_all
      "SELECT @int32{id}, @string{name}, @Phone?{phone} FROM users WHERE id IN \
       (%list{%int32{id}})"]
    dbh
    ids
  >>| List.map user_of_tuple
;;

let get_user dbh ~id =
  [%mysql
    select_one
      "SELECT @int32{id}, @string{name}, @Phone?{phone} FROM users WHERE id = %int32{id}"]
    dbh
    ~id
  >>| user_of_tuple
;;

let insert_user =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES (%int32{id}, %string{name}, \
       %Phone?{phone})"]
;;

let insert_users =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES %list{(%int32{id}, %string{name}, \
       %Phone?{phone})}"]
;;

let update_user =
  [%mysql
    execute
      "UPDATE users SET name = %string{name}, phone = %Phone?{phone} WHERE id = \
       %int32{id}"]
;;

let delete_user = [%mysql execute "DELETE FROM users WHERE id = %int32{id}"]

(** Main functions and values. *)

let test dbh =
  let open IO_result in
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
  Printf.printf "All users:\n";
  List.iter print_user users;
  get_some_users dbh [ 1l; 2l; 3l ]
  >>= fun users ->
  Printf.printf "Users with ID in {1, 2, 3}:\n";
  List.iter print_user users;
  update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321")
  >>= fun () ->
  get_user dbh ~id:2l
  >>= fun user ->
  Printf.printf "User with ID = 2 after update:\n";
  print_user user;
  delete_user dbh ~id:3l
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Printf.printf "All users after deleting one with ID = 3:\n";
  List.iter print_user users;
  Ok ()
;;

let main () =
  let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
  let wrapped_dbh = Prepared.init dbh in
  let res = test wrapped_dbh in
  Mysql.disconnect dbh;
  match res with
  | Ok () -> Printf.printf "All went well!\n"
  | Error _ -> Printf.printf "An error occurred!\n"
;;

let () = main ()
