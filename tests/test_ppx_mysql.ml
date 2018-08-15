(********************************************************************************)
(*  Test_ppx_mysql.ml                                                           *)
(********************************************************************************)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type param = Ppx_mysql.param =
    {
    typ: string;
    opt: bool;
    name: string;
    of_string: string;
    to_string: string;
    } [@@deriving eq, show]

type parsed_query = Ppx_mysql.parsed_query =
    {
    query: string;
    in_params: param list;
    out_params: param list;
    } [@@deriving eq, show]

type parse_error =
    [ `Bad_param of string
    | `Escape_at_end
    | `Unknown_mysql_type of string
    | `Unterminated_string
    ] [@@deriving eq, show]


(********************************************************************************)
(** {1 TESTABLE modules}                                                        *)
(********************************************************************************)

let param_mod = Alcotest.testable pp_param equal_param

let parsed_query_mod = Alcotest.testable pp_parsed_query equal_parsed_query

let parse_error_mod = Alcotest.testable pp_parse_error equal_parse_error


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let query_0 = "SELECT true"
let parsed_query_0 =
    {
    query = "SELECT true";
    in_params = [];
    out_params = [];
    }

let query_out1 = "SELECT @INT{id} FROM users"
let parsed_query_out1 =
    {
    query = "SELECT id FROM users";
    in_params = [];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    }

let query_out2 = "SELECT @INT{id}, @TEXT{name} FROM users"
let parsed_query_out2 =
    {
    query = "SELECT id, name FROM users";
    in_params = [];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_out3 = "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users"
let parsed_query_out3 =
    {
    query = "SELECT id, name, phone FROM users";
    in_params = [];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_in1 = "INSERT INTO users (id) VALUES (%INT{id})"
let parsed_query_in1 =
    {
    query = "INSERT INTO users (id) VALUES (?)";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    out_params = [];
    }

let query_in2 = "INSERT INTO users (id, name) VALUES (%INT{id}, %TEXT{name})"
let parsed_query_in2 =
    {
    query = "INSERT INTO users (id, name) VALUES (?, ?)";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    out_params = [];
    }

let query_in3 = "INSERT INTO users (id, name, phone) VALUES (%INT{id}, %TEXT{name}, %TEXT?{phone})"
let parsed_query_in3 =
    {
    query = "INSERT INTO users (id, name, phone) VALUES (?, ?, ?)";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    out_params = [];
    }

let query_inout = "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users WHERE id = %INT{id} OR name = %TEXT{name} OR PHONE = %TEXT?{phone}"
let parsed_query_inout =
    {
    query = "SELECT id, name, phone FROM users WHERE id = ? OR name = ? OR PHONE = ?";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_quoted0 = "SELECT @INT{id}, @TEXT{name} FROM users WHERE id = %INT{id} OR NAME = 'Hello @INT{name} world'"
let parsed_query_quoted0 =
    {
    query = "SELECT id, name FROM users WHERE id = ? OR NAME = 'Hello @INT{name} world'";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_quoted1 = "SELECT @INT{id}, @TEXT{name} FROM users WHERE id = %INT{id} OR NAME = \"Hello @INT{name} world\""
let parsed_query_quoted1 =
    {
    query = "SELECT id, name FROM users WHERE id = ? OR NAME = \"Hello @INT{name} world\"";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_quoted2 = "SELECT @INT{id}, @TEXT{name} FROM users WHERE id = %INT{id} OR NAME = 'Hello ''@INT{name}'' world'"
let parsed_query_quoted2 =
    {
    query = "SELECT id, name FROM users WHERE id = ? OR NAME = 'Hello ''@INT{name}'' world'";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_quoted3 = "SELECT @INT{id}, @TEXT{name} FROM users WHERE id = %INT{id} OR NAME = \"Hello '@INT{name}' world\""
let parsed_query_quoted3 =
    {
    query = "SELECT id, name FROM users WHERE id = ? OR NAME = \"Hello '@INT{name}' world\"";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_runtime.identity"; to_string = "Ppx_mysql_runtime.identity"};
        ];
    }

let query_bad0 = "SELECT @INT{}"
let error_bad0 = `Bad_param "@INT{}"

let query_bad1 = "SELECT %INT{}"
let error_bad1 = `Bad_param "%INT{}"

let query_bad2 = "SELECT true\\"
let error_bad2 = `Escape_at_end

let query_bad3 = "SELECT @FOO{id} FROM users"
let error_bad3 = `Unknown_mysql_type "FOO"

let query_bad4 = "SELECT id, name FROM users WHERE id = %FOO{id}"
let error_bad4 = `Unknown_mysql_type "FOO"

let query_bad5 = "SELECT 'hello"
let error_bad5 = `Unterminated_string

let query_bad6 = "SELECT \"hello"
let error_bad6 = `Unterminated_string

let test_parse_query () =
    let run desc query expected =
        Alcotest.(check (result parsed_query_mod parse_error_mod) desc expected (Ppx_mysql.parse_query query)) in
    run "query_0" query_0 (Ok parsed_query_0);
    run "query_out1" query_out1 (Ok parsed_query_out1);
    run "query_out2" query_out2 (Ok parsed_query_out2);
    run "query_out3" query_out3 (Ok parsed_query_out3);
    run "query_in1" query_in1 (Ok parsed_query_in1);
    run "query_in2" query_in2 (Ok parsed_query_in2);
    run "query_in3" query_in3 (Ok parsed_query_in3);
    run "query_inout" query_inout (Ok parsed_query_inout);
    run "query_quoted0" query_quoted0 (Ok parsed_query_quoted0);
    run "query_quoted1" query_quoted1 (Ok parsed_query_quoted1);
    run "query_quoted2" query_quoted2 (Ok parsed_query_quoted2);
    run "query_quoted3" query_quoted3 (Ok parsed_query_quoted3);
    run "query_bad0" query_bad0 (Error error_bad0);
    run "query_bad1" query_bad1 (Error error_bad1);
    run "query_bad2" query_bad2 (Error error_bad2);
    run "query_bad3" query_bad3 (Error error_bad3);
    run "query_bad4" query_bad4 (Error error_bad4);
    run "query_bad5" query_bad5 (Error error_bad5);
    run "query_bad6" query_bad6 (Error error_bad6)

let testset =
    [
    ("parse_query", `Quick, test_parse_query);
    ]


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let () = Alcotest.run "Ppx_mysql module"
    [
    ("Ppx_mysql", testset);
    ]