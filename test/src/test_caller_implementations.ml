open! Core
open! Async
open! Import

let test mode =
  match mode with
  | `Sexp ->
    Process.run_exn
      ~prog:"../bin/main.exe"
      ~args:[ "caller-implementations"; "-sexp" ]
      ~stdin:
        (let rpc_name =
           Rpc.Rpc.name Command_rpc_test_protocol.Caller_implementations_rpcs.Callee.rpc
         in
         let version =
           Rpc.Rpc.version
             Command_rpc_test_protocol.Caller_implementations_rpcs.Callee.rpc
         in
         let query = [%sexp (() : unit)] in
         Sexp.to_string_mach [%sexp { rpc_name : string; version : int; query : Sexp.t }])
      ()
    >>| print_endline
  | `Bin_io implementations ->
    let%bind connection =
      Command_rpc.Connection.create
        ~implementations
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
  Rpc.Implementations.create_exn ~implementations:[] ~on_unknown_rpc
;;

let%expect_test _ =
  let%bind () = test `Sexp in
  [%expect
    {| (Error"I can't know what the secret number is because I was invoked in sexp mode") |}];
  let%bind () =
    test
      (`Bin_io
         Command_rpc_test_protocol.Caller_implementations_rpcs.Caller.implementations)
  in
  [%expect {| (Ok (Ok "The secret number is 42")) |}];
  let%bind () = test (`Bin_io (null_implementations ~on_unknown_rpc:`Continue)) in
  [%expect
    {|
    (Ok (
      Error (
        (rpc_error (Unimplemented_rpc caller_rpc (Version 1)))
        (connection_description <created-directly>)
        (rpc_tag                caller_rpc)
        (rpc_version            1)))) |}];
  (* Note that the error is on the *caller* side. *)
  let%bind () = test (`Bin_io (null_implementations ~on_unknown_rpc:`Close_connection)) in
  [%expect
    {|
    (Error (
      (rpc_error (Connection_closed ("Rpc message handling loop stopped")))
      (connection_description <created-directly>)
      (rpc_tag                callee_rpc)
      (rpc_version            1))) |}];
  (* Note that the error is on the *caller* side. *)
  let%bind () = test (`Bin_io (null_implementations ~on_unknown_rpc:`Raise)) in
  [%expect
    {|
    (Error (
      (rpc_error (
        Uncaught_exn (
          monitor.ml.Error
          (rpc_error.ml.Rpc
            (Unimplemented_rpc caller_rpc (Version 1))
            <created-directly>)
          ("<backtrace elided in test>" "Caught by monitor RPC connection loop"))))
      (connection_description <created-directly>)
      (rpc_tag                callee_rpc)
      (rpc_version            1))) |}];
  return ()
;;
