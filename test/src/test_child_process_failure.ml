open! Core
open! Async
open! Import

let () = Dynamic.set_root Backtrace.elide true

let%expect_test _ =
  let%bind connection_or_error =
    Command_rpc.Connection.create
      ~wait_for_stderr_transfer:false
      ~prog:"bash"
      ~args:[ "-c"; "exit 1" ]
      ()
  in
  print_s [%sexp (connection_or_error : (_, Error.t) Result.t)];
  [%expect
    {|
    (Error (
      handshake_error.ml.Handshake_error (
        (Eof_during_step Header)
        ("Command_rpc client (parent process)"
          (prog      <hidden_in_test>)
          (args      <hidden_in_test>)
          (child_pid <hidden_in_test>)))))
    |}];
  return ()
;;

let test_exit_zero () =
  Command_rpc.Connection.with_close
    (fun conn ->
      Rpc.Rpc.dispatch
        Command_rpc_test_protocol.Exit_zero_rpc.rpc
        (Command_rpc.Connection.rpc_connection conn)
        ())
    ~wait_for_stderr_transfer:false
    ~prog:"../bin/main.exe"
    ~args:[ "exit-zero" ]
;;

let%expect_test _ =
  let%bind res = test_exit_zero () in
  print_s
    (Expect_test_helpers_base.smash_sexp
       [%sexp (res : Nothing.t Or_error.t)]
       ~f:(function
         | List (Atom "Connection_closed" :: _reason) ->
           List [ Atom "Connection_closed"; Atom "_" ]
         | s -> s));
  [%expect
    {|
    (Error (
      (rpc_error (Connection_closed _))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    exit_zero)
      (rpc_version 1)))
    |}];
  return ()
;;
