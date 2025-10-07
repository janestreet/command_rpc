open! Core
open! Async
open! Import

let () = Dynamic.set_root Backtrace.elide true

let test new_fds_for_rpc mode =
  match mode with
  | `Sexp ->
    if new_fds_for_rpc
    then raise_s [%message "Sexp mode with separate read and write FDs is not supported"];
    let sexp_string =
      let rpc_name =
        Rpc.Rpc.name Command_rpc_test_protocol.Caller_implementations_rpcs.Callee.rpc
      in
      let version =
        Rpc.Rpc.version Command_rpc_test_protocol.Caller_implementations_rpcs.Callee.rpc
      in
      let query = [%sexp (() : unit)] in
      Sexp.to_string_mach [%sexp { rpc_name : string; version : int; query : Sexp.t }]
    in
    Process.run_exn
      ~prog:"../bin/main.exe"
      ~args:[ "caller-implementations"; "-sexp" ]
      ~stdin:sexp_string
      ()
    >>| print_endline
  | `Bin_io implementations ->
    let%bind connection =
      Command_rpc.Connection.create
        ~new_fds_for_rpc
        ~implementations
        ~wait_for_stderr_transfer:false
        ~prog:"../bin/main.exe"
        ~args:[ "caller-implementations" ]
        ()
      >>| ok_exn
      >>| Command_rpc.Connection.rpc_connection
    in
    let%bind response_or_rpc_error =
      Rpc.Rpc.dispatch
        Command_rpc_test_protocol.Caller_implementations_rpcs.Callee.rpc
        connection
        ()
    in
    let%bind () = Rpc.Connection.close connection in
    print_s [%sexp (response_or_rpc_error : string Or_error.t Or_error.t)];
    return ()
;;

let null_implementations ~on_unknown_rpc =
  Rpc.Implementations.create_exn
    ~implementations:[]
    ~on_unknown_rpc
    ~on_exception:Log_on_background_exn
;;

let%expect_test _ =
  let%bind () = test false `Sexp in
  [%expect
    {| (Error"I can't know what the secret number is because I was invoked in sexp mode") |}];
  let%bind () =
    test
      false
      (`Bin_io
        Command_rpc_test_protocol.Caller_implementations_rpcs.Caller.implementations)
  in
  let%bind () =
    test
      true
      (`Bin_io
        Command_rpc_test_protocol.Caller_implementations_rpcs.Caller.implementations)
  in
  [%expect
    {|
    (Ok (Ok "The secret number is 42"))
    (Ok (Ok "The secret number is 42"))
    |}];
  let%bind () = test false (`Bin_io (null_implementations ~on_unknown_rpc:`Continue)) in
  let%bind () = test true (`Bin_io (null_implementations ~on_unknown_rpc:`Continue)) in
  [%expect
    {|
    (Ok (
      Error (
        (rpc_error (Unimplemented_rpc caller_rpc (Version 1)))
        (connection_description (
          "Command_rpc server (child process)"
          (executable <hidden_in_test>)
          (pid        <hidden_in_test>)
          (parent_pid <hidden_in_test>)))
        (rpc_name    caller_rpc)
        (rpc_version 1))))
    (Ok (
      Error (
        (rpc_error (Unimplemented_rpc caller_rpc (Version 1)))
        (connection_description (
          "Command_rpc server (child process)"
          (executable <hidden_in_test>)
          (pid        <hidden_in_test>)
          (parent_pid <hidden_in_test>)))
        (rpc_name    caller_rpc)
        (rpc_version 1))))
    |}];
  (* Note that the error is on the *caller* side. *)
  let%bind () =
    test false (`Bin_io (null_implementations ~on_unknown_rpc:`Close_connection))
  in
  let%bind () =
    test true (`Bin_io (null_implementations ~on_unknown_rpc:`Close_connection))
  in
  [%expect
    {|
    (Error (
      (rpc_error (
        Connection_closed ((
          "Rpc message handling loop stopped" (
            connection_description (
              "Command_rpc client (parent process)"
              (prog      <hidden_in_test>)
              (args      <hidden_in_test>)
              (child_pid <hidden_in_test>)))))))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    callee_rpc)
      (rpc_version 1)))
    (Error (
      (rpc_error (
        Connection_closed ((
          "Rpc message handling loop stopped" (
            connection_description (
              "Command_rpc client (parent process)"
              (prog      <hidden_in_test>)
              (args      <hidden_in_test>)
              (child_pid <hidden_in_test>)))))))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    callee_rpc)
      (rpc_version 1)))
    |}];
  (* Note that the error is on the *caller* side. *)
  let%bind () = test false (`Bin_io (null_implementations ~on_unknown_rpc:`Raise)) in
  let%bind () = test true (`Bin_io (null_implementations ~on_unknown_rpc:`Raise)) in
  [%expect
    {|
    (Error (
      (rpc_error (
        Uncaught_exn (
          monitor.ml.Error
          (rpc_error.ml.Rpc
            (Unimplemented_rpc caller_rpc (Version 1))
            ("Command_rpc client (parent process)"
              (prog      <hidden_in_test>)
              (args      <hidden_in_test>)
              (child_pid <hidden_in_test>)))
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    callee_rpc)
      (rpc_version 1)))
    (Error (
      (rpc_error (
        Uncaught_exn (
          monitor.ml.Error
          (rpc_error.ml.Rpc
            (Unimplemented_rpc caller_rpc (Version 1))
            ("Command_rpc client (parent process)"
              (prog      <hidden_in_test>)
              (args      <hidden_in_test>)
              (child_pid <hidden_in_test>)))
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    callee_rpc)
      (rpc_version 1)))
    |}];
  return ()
;;
