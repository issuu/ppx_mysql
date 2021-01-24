open Async
open Core

include Ppx_mysql_runtime.Make_context (struct
  module IO = struct
    type 'a t = 'a Deferred.t

    let return = Deferred.return
    let bind x f = Deferred.bind x ~f
  end

  module Prepared = struct
    type dbh = Mysql.dbd
    type stmt = Mysql.Prepared.stmt
    type stmt_result = Mysql.Prepared.stmt_result

    let create dbh sql =
      Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.create dbh sql))
    ;;

    let close stmt =
      Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.close stmt))
    ;;

    let execute_null stmt args =
      Deferred.(
        Or_error.try_with (fun () -> return @@ (Mysql.Prepared.execute_null stmt) args))
    ;;

    let fetch stmt_res =
      Deferred.(Or_error.try_with (fun () -> return @@ Mysql.Prepared.fetch stmt_res))
    ;;
  end
end)
