open! Core
open! Async
open! Import

let test new_fds_for_rpc n =
  let get_all_responses conn =
    let rpc = Command_rpc_test_protocol.Simple_state_rpc.rpc in
    let%bind.Deferred.Or_error initial_state, updates, metadata =
      Rpc.State_rpc.dispatch rpc (Command_rpc.Connection.rpc_connection conn) n
      >>| Or_error.join
    in
    let%bind updates = Pipe.to_list updates in
    match%map Rpc.State_rpc.close_reason metadata with
    | Closed_locally -> assert false
    | Closed_remotely -> Ok (initial_state, updates)
    | Error e -> Error e
  in
  let%bind result =
    Command_rpc.Connection.with_close
      get_all_responses
      ~new_fds_for_rpc
      ~wait_for_stderr_transfer:false
      ~prog:"../bin/main.exe"
      ~args:[ "simple-state" ]
  in
  let initial_state, updates = ok_exn result in
  print_s [%message (initial_state : string) (updates : int list)];
  Deferred.unit
;;

let%expect_test "non-versioned state rpc" =
  let%bind () = test false 3 in
  let%bind () = test true 3 in
  [%expect
    {|
    ((initial_state "got 3 items") (updates (0 1 2)))
    ((initial_state "got 3 items") (updates (0 1 2)))
    |}];
  return ()
;;

let%expect_test "non-versioned state rpc with zero updates" =
  let%bind () = test false 0 in
  let%bind () = test true 0 in
  [%expect
    {|
    ((initial_state "got 0 items") (updates ()))
    ((initial_state "got 0 items") (updates ()))
    |}];
  return ()
;;
