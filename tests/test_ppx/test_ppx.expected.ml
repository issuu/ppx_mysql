let test_no_params dbh =
  let open IO_result in
  IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ("SELECT TRUE", [||]))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 0))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_single_output_params dbh =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok ("SELECT name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 0))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 1))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_pair_output_params dbh =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok ("SELECT id, name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_one_input_params dbh ~(id : int) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT name FROM users WHERE id = ?"
       , [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 0))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 1))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_two_input_pair_output_params dbh ~(id : int) ~(name : string) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT id, name FROM users WHERE id = ? OR name = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
          ; Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity name) |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_select_all dbh =
  let open IO_result in
  IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ("SELECT id, name FROM users", [||]))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
        loop [] )
        () )

let test_repeated_input_params dbh ~(id : int) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT id, name FROM users WHERE id <> ? AND id <> ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
          ; Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id) |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
        loop [] )
        () )

let test_select_opt dbh ~(id : int) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT id, name FROM users WHERE id = ?"
       , [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
                (Ppx_mysql_runtime.Stdlib.Result.Ok Ppx_mysql_runtime.Stdlib.Option.None)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return
                (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_maybe_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return
                (Ppx_mysql_runtime.Stdlib.Result.Ok
                   (Ppx_mysql_runtime.Stdlib.Option.Some hd))
        in
        loop [] )
        () )

let test_execute dbh ~(id : int) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "DELETE FROM users WHERE id = ?"
       , [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 0))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result
        >>= function
        | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
        | Ppx_mysql_runtime.Stdlib.Option.None ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ()) )
        () )

let test_int dbh ~(a : int) ~(b : int option) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int a)
          ; (Ppx_mysql_runtime.Stdlib.Option.map Pervasives.string_of_int) b |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("a", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "a") )
          , let deserialize value =
              try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                raise
                  (Deserialization_error ("b", "Ppx_mysql_runtime.int_of_string", value))
            in
            Ppx_mysql_runtime.Stdlib.Option.map
              deserialize
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_int32 dbh ~(a : int32) ~(b : int32 option) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Int32.to_string a)
          ; (Ppx_mysql_runtime.Stdlib.Option.map Int32.to_string) b |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int32_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("a", "Ppx_mysql_runtime.int32_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "a") )
          , let deserialize value =
              try Ppx_mysql_runtime.int32_of_string value with Failure _ ->
                raise
                  (Deserialization_error ("b", "Ppx_mysql_runtime.int32_of_string", value))
            in
            Ppx_mysql_runtime.Stdlib.Option.map
              deserialize
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_int64 dbh ~(a : int64) ~(b : int64 option) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Int64.to_string a)
          ; (Ppx_mysql_runtime.Stdlib.Option.map Int64.to_string) b |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int64_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("a", "Ppx_mysql_runtime.int64_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "a") )
          , let deserialize value =
              try Ppx_mysql_runtime.int64_of_string value with Failure _ ->
                raise
                  (Deserialization_error ("b", "Ppx_mysql_runtime.int64_of_string", value))
            in
            Ppx_mysql_runtime.Stdlib.Option.map
              deserialize
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_bool dbh ~(a : bool) ~(b : bool option) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_bool a)
          ; (Ppx_mysql_runtime.Stdlib.Option.map Pervasives.string_of_bool) b |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.bool_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("a", "Ppx_mysql_runtime.bool_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "a") )
          , let deserialize value =
              try Ppx_mysql_runtime.bool_of_string value with Failure _ ->
                raise
                  (Deserialization_error ("b", "Ppx_mysql_runtime.bool_of_string", value))
            in
            Ppx_mysql_runtime.Stdlib.Option.map
              deserialize
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_string dbh ~(a : string) ~(b : string option) =
  let open IO_result in
  IO.return
    (Ppx_mysql_runtime.Stdlib.Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?"
       , [| Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity a)
          ; (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity) b |] ))
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.identity value with Failure _ ->
                       raise
                         (Deserialization_error ("a", "Ppx_mysql_runtime.identity", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "a") )
          , let deserialize value =
              try Ppx_mysql_runtime.identity value with Failure _ ->
                raise (Deserialization_error ("b", "Ppx_mysql_runtime.identity", value))
            in
            Ppx_mysql_runtime.Stdlib.Option.map
              deserialize
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
          | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
          | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
              IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
        in
        loop [] )
        () )

let test_list0 dbh elems =
  let open IO_result in
  ( match elems with
  | [] ->
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Empty_input_list)
  | elems ->
      let subsqls = Ppx_mysql_runtime.Stdlib.List.map (fun _ -> "?") elems in
      let patch = Ppx_mysql_runtime.Stdlib.String.concat ", " subsqls in
      let sql =
        Ppx_mysql_runtime.Stdlib.String.append
          "SELECT id, name FROM users WHERE id IN ("
          (Ppx_mysql_runtime.Stdlib.String.append patch ")")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun id ->
                  [Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)] )
                elems))
      in
      let params = Ppx_mysql_runtime.Stdlib.Array.concat [[||]; params_between; [||]] in
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
        loop [] )
        () )

let test_list1 dbh elems =
  let open IO_result in
  ( match elems with
  | [] ->
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Empty_input_list)
  | elems ->
      let subsqls = Ppx_mysql_runtime.Stdlib.List.map (fun _ -> "(?, ?, NULL)") elems in
      let patch = Ppx_mysql_runtime.Stdlib.String.concat ", " subsqls in
      let sql =
        Ppx_mysql_runtime.Stdlib.String.append
          "INSERT INTO users (id, name, phone) VALUES "
          (Ppx_mysql_runtime.Stdlib.String.append patch "")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun (id, name) ->
                  [ Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
                  ; Ppx_mysql_runtime.Stdlib.Option.Some
                      (Ppx_mysql_runtime.identity name) ] )
                elems))
      in
      let params = Ppx_mysql_runtime.Stdlib.Array.concat [[||]; params_between; [||]] in
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 0))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result
        >>= function
        | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
        | Ppx_mysql_runtime.Stdlib.Option.None ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ()) )
        () )

let test_list2 dbh elems ~(name : string) ~(age : int) =
  let open IO_result in
  ( match elems with
  | [] ->
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Empty_input_list)
  | elems ->
      let subsqls = Ppx_mysql_runtime.Stdlib.List.map (fun _ -> "?") elems in
      let patch = Ppx_mysql_runtime.Stdlib.String.concat ", " subsqls in
      let sql =
        Ppx_mysql_runtime.Stdlib.String.append
          "SELECT id, name FROM users WHERE name = ? OR id IN ("
          (Ppx_mysql_runtime.Stdlib.String.append patch ") OR age > ?")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun id ->
                  [Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)] )
                elems))
      in
      let params =
        Ppx_mysql_runtime.Stdlib.Array.concat
          [ [|Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity name)|]
          ; params_between
          ; [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int age)|] ]
      in
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( ( try
                Ppx_mysql_runtime.Stdlib.Option.get
                  (let deserialize value =
                     try Ppx_mysql_runtime.int_of_string value with Failure _ ->
                       raise
                         (Deserialization_error
                            ("id", "Ppx_mysql_runtime.int_of_string", value))
                   in
                   Ppx_mysql_runtime.Stdlib.Option.map
                     deserialize
                     (Ppx_mysql_runtime.Stdlib.Array.get row 0))
              with Invalid_argument _ -> raise (Expected_non_null_column "id") )
          , try
              Ppx_mysql_runtime.Stdlib.Option.get
                (let deserialize value =
                   try Ppx_mysql_runtime.identity value with Failure _ ->
                     raise
                       (Deserialization_error
                          ("name", "Ppx_mysql_runtime.identity", value))
                 in
                 Ppx_mysql_runtime.Stdlib.Option.map
                   deserialize
                   (Ppx_mysql_runtime.Stdlib.Array.get row 1))
            with Invalid_argument _ -> raise (Expected_non_null_column "name") )
      with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 2))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
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
        loop [] )
        () )

let test_list3 dbh elems =
  let open IO_result in
  ( match elems with
  | [] ->
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Empty_input_list)
  | elems ->
      let subsqls = Ppx_mysql_runtime.Stdlib.List.map (fun _ -> "(?, ?, ?, ?)") elems in
      let patch = Ppx_mysql_runtime.Stdlib.String.concat ", " subsqls in
      let sql =
        Ppx_mysql_runtime.Stdlib.String.append
          "INSERT INTO users (id, name, real_name, age) VALUES "
          (Ppx_mysql_runtime.Stdlib.String.append patch "")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun (id, name, age) ->
                  [ Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
                  ; Ppx_mysql_runtime.Stdlib.Option.Some
                      (Ppx_mysql_runtime.identity name)
                  ; Ppx_mysql_runtime.Stdlib.Option.Some
                      (Ppx_mysql_runtime.identity name)
                  ; Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int age)
                  ] )
                elems))
      in
      let params = Ppx_mysql_runtime.Stdlib.Array.concat [[||]; params_between; [||]] in
      IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let[@warning "-26"] process_out_params row =
    (let exception Deserialization_error of string * string * string in
    (let exception Expected_non_null_column of string in
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Deserialization_error (col, f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Deserialization_error (f, v)])
      | Expected_non_null_column col ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Column_errors [col, `Expected_non_null_value])
    else
      Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_columns (len_row, 0))) 
    [@warning "-38"]) [@warning "-38"]
  in
  Prepared.with_stmt dbh sql (fun stmt ->
      Prepared.execute_null stmt params
      >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result
        >>= function
        | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
        | Ppx_mysql_runtime.Stdlib.Option.None ->
            IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ()) )
        () )
