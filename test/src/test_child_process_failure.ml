open! Core
open! Async
open! Import

let () = Backtrace.elide := true

let%expect_test _ =
  let%bind connection_or_error =
    Command_rpc.Connection.create
      ~prog:
        "bash"
      ~args:[ "-c"; "exit 1" ]
      ()
  in
  print_s [%sexp (connection_or_error : (_, Error.t) Result.t)];
  [%expect
    {|
    (Error (
      connection.ml.Handshake_error.Handshake_error (Eof <created-directly>))) |}];
  return ()
;;

let test_exit_zero () =
  Command_rpc.Connection.with_close
    (fun conn ->
       Rpc.Rpc.dispatch
         Command_rpc_test_protocol.Exit_zero_rpc.rpc
         (Command_rpc.Connection.rpc_connection conn)
         ())
    ~prog:"../bin/main.exe"
    ~args:[ "exit-zero" ]
;;

let%expect_test _ =
  let%bind res = test_exit_zero () in
  print_s [%sexp (res : Nothing.t Or_error.t)];
  [%expect
    {|
    (Error (
      (rpc_error (Connection_closed ("EOF or connection closed")))
      (connection_description <created-directly>)
      (rpc_name               exit_zero)
      (rpc_version            1))) |}];
  return ()
;;