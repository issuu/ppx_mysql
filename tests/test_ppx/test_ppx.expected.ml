let test_no_params dbh =
  let open Async.Deferred.Or_error in
  Async.return (Result.Ok ("SELECT TRUE", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 0
    then Result.Ok ()
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 0)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_single_output_params dbh =
  let open Async.Deferred.Or_error in
  Async.return (Result.Ok ("SELECT name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 1
    then (
      let err_accum = [] in
      match
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      with
      | Some res, _ -> Result.Ok res
      | None, err -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 1)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_pair_output_params dbh =
  let open Async.Deferred.Or_error in
  Async.return (Result.Ok ("SELECT id, name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_one_input_params dbh ~(id : int) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT name FROM users WHERE id = ?"
       , [| Base.Option.Some (Stdlib.string_of_int id) |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 1
    then (
      let err_accum = [] in
      match
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      with
      | Some res, _ -> Result.Ok res
      | None, err -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 1)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_two_input_pair_output_params dbh ~(id : int) ~(name : string) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id = ? OR name = ?"
       , [| Base.Option.Some (Stdlib.string_of_int id)
          ; Base.Option.Some (Ppx_mysql_runtime.identity name)
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_select_all dbh =
  let open Async.Deferred.Or_error in
  Async.return (Result.Ok ("SELECT id, name FROM users", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
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
;;

let test_repeated_input_params dbh ~(id : int) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id <> ? AND id <> ?"
       , [| Base.Option.Some (Stdlib.string_of_int id)
          ; Base.Option.Some (Stdlib.string_of_int id)
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
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
;;

let test_select_opt dbh ~(id : int) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id = ?"
       , [| Base.Option.Some (Stdlib.string_of_int id) |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None -> Async.return (Result.Ok None)
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_maybe_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok (Some hd))
        in
        loop [])
        ())
;;

let test_execute dbh ~(id : int) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ("DELETE FROM users WHERE id = ?", [| Base.Option.Some (Stdlib.string_of_int id) |]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 0
    then Result.Ok ()
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 0)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Ppx_mysql_runtime.Prepared.fetch stmt_result
        >>= function
        | Some _ -> Async.return (Base.Or_error.error_string "Expected_none_found_one")
        | None -> Async.return (Result.Ok ()))
        ())
;;

let test_int dbh ~(a : int) ~(b : int option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Stdlib.string_of_int a)
          ; (Base.Option.map ~f:Stdlib.string_of_int) b
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_int32 dbh ~(a : int32) ~(b : int32 option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Int32.to_string a)
          ; (Base.Option.map ~f:Int32.to_string) b
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int32_of_string
          "Ppx_mysql_runtime.int32_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int32_of_string
          "Ppx_mysql_runtime.int32_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_int64 dbh ~(a : int64) ~(b : int64 option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Int64.to_string a)
          ; (Base.Option.map ~f:Int64.to_string) b
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int64_of_string
          "Ppx_mysql_runtime.int64_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int64_of_string
          "Ppx_mysql_runtime.int64_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_bool dbh ~(a : bool) ~(b : bool option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Stdlib.string_of_bool a)
          ; (Base.Option.map ~f:Stdlib.string_of_bool) b
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.bool_of_string
          "Ppx_mysql_runtime.bool_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.bool_of_string
          "Ppx_mysql_runtime.bool_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_string dbh ~(a : string) ~(b : string option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Ppx_mysql_runtime.identity a)
          ; (Base.Option.map ~f:Ppx_mysql_runtime.identity) b
         |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_custom_type dbh ~(a : Id.t) ~(b : Phone.t option) =
  let open Async.Deferred.Or_error in
  Async.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Base.Option.Some (Id.to_string a); (Base.Option.map ~f:Phone.to_string) b |]
       ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Id.of_string
          "Id.of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Phone.of_string
          "Phone.of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Ppx_mysql_runtime.Prepared.fetch stmt_result
          >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Some row ->
            (match process_out_params row with
            | Result.Ok row' -> loop [ row' ]
            | Result.Error _ as err -> Async.return err)
          | [], None ->
            Async.return (Base.Or_error.error_string "Expected_one_found_none")
          | _ :: _, Some _ ->
            Async.return (Base.Or_error.error_string "Expected_one_found_many")
          | hd :: _, None -> Async.return (Result.Ok hd)
        in
        loop [])
        ())
;;

let test_list0 dbh elems =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "?") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "SELECT id, name FROM users WHERE id IN (" ^ patch ^ ")"
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun id -> [ Base.Option.Some (Stdlib.string_of_int id) ])
              elems))
    in
    let params = Base.Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
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
;;

let test_list1 dbh elems =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "(?, ?, NULL)") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "INSERT INTO users (id, name, phone) VALUES " ^ patch ^ ""
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun (id, name) ->
                [ Base.Option.Some (Stdlib.string_of_int id)
                ; Base.Option.Some (Ppx_mysql_runtime.identity name)
                ])
              elems))
    in
    let params = Base.Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 0
    then Result.Ok ()
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 0)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Ppx_mysql_runtime.Prepared.fetch stmt_result
        >>= function
        | Some _ -> Async.return (Base.Or_error.error_string "Expected_none_found_one")
        | None -> Async.return (Result.Ok ()))
        ())
;;

let test_list2 dbh elems ~(name : string) ~(age : int) =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "?") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "SELECT id, name FROM users WHERE name = ? OR id IN (" ^ patch ^ ") OR age > ?"
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun id -> [ Base.Option.Some (Stdlib.string_of_int id) ])
              elems))
    in
    let params =
      Base.Array.concat
        [ [| Base.Option.Some (Ppx_mysql_runtime.identity name) |]
        ; params_between
        ; [| Base.Option.Some (Stdlib.string_of_int age) |]
        ]
    in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
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
;;

let test_list3 dbh elems =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "(?, ?, ?, ?)") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "INSERT INTO users (id, name, real_name, age) VALUES " ^ patch ^ ""
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun (id, name, age) ->
                [ Base.Option.Some (Stdlib.string_of_int id)
                ; Base.Option.Some (Ppx_mysql_runtime.identity name)
                ; Base.Option.Some (Ppx_mysql_runtime.identity name)
                ; Base.Option.Some (Stdlib.string_of_int age)
                ])
              elems))
    in
    let params = Base.Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 0
    then Result.Ok ()
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 0)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_cached dbh sql (fun stmt ->
      Ppx_mysql_runtime.Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Ppx_mysql_runtime.Prepared.fetch stmt_result
        >>= function
        | Some _ -> Async.return (Base.Or_error.error_string "Expected_none_found_one")
        | None -> Async.return (Result.Ok ()))
        ())
;;

let test_cached0 dbh elems =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "?") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "SELECT id, name FROM users WHERE id IN (" ^ patch ^ ")"
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun id -> [ Base.Option.Some (Stdlib.string_of_int id) ])
              elems))
    in
    let params = Base.Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
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
;;

let test_cached1 dbh elems =
  let open Async.Deferred.Or_error in
  (match elems with
  | [] -> Async.return @@ Base.Or_error.error_string "Empty_input_list"
  | elems ->
    let subsqls = Base.List.map ~f:(fun _ -> "?") elems in
    let patch = String.concat ~sep:", " subsqls in
    let sql =
      let open String in
      "SELECT id, name FROM users WHERE id IN (" ^ patch ^ ")"
    in
    let params_between =
      Base.Array.of_list
        (Base.List.concat
           (Base.List.map
              ~f:(fun id -> [ Base.Option.Some (Stdlib.string_of_int id) ])
              elems))
    in
    let params = Base.Array.concat [ [||]; params_between; [||] ] in
    Async.return (Result.Ok (sql, params)))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Base.( = ) len_row 2
    then (
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
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
      match col0, col1 with
      | Base.Option.Some v0, Base.Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Base.Or_error.error_s @@ Ppx_mysql_runtime.sexp_of_column_errors err_accum)
    else
      Base.Or_error.error_s
      @@ Ppx_mysql_runtime.sexp_of_unexpected_number_of_columns (len_row, 2)
    [@@warning "-26"]
  in
  Ppx_mysql_runtime.Prepared.with_stmt_uncached dbh sql (fun stmt ->
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
;;
