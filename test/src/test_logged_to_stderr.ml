open! Core
open! Async
open! Import

let _ = Command_rpc_test_protocol.Rpc_logged_to_stderr.rpc_one_way


let test n =
  let dispatch_rpc conn =
    let rpc = Command_rpc_test_protocol.Rpc_logged_to_stderr.rpc in
    Rpc.Rpc.dispatch rpc (Command_rpc.Connection.rpc_connection conn) n
  in
  Command_rpc.Connection.with_close
    dispatch_rpc
    ~wait_for_stderr_transfer:true
    ~prog:"../bin/main.exe"
    ~args:([ [ "logged-to-stderr" ] ] |> List.concat)
  |> Deferred.map ~f:ok_exn
;;

let%expect_test _ =
  let%bind () = test 0 in
  [%expect {| 0 |}];
  let%bind () = test 5 in
  [%expect {| 5 |}];
  let%bind () = test 100 in
  [%expect {| 100 |}];
  return ()
;;
