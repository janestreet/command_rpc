open! Core
open! Async
open! Import

module Versions = struct
  type t =
    { client : int
    ; server_min : int
    ; server_max : int
    }
end

let test new_fds_for_rpc (versions : Versions.t) =
  let n = 3 in
  let get_all_responses conn =
    let rpc =
      Command_rpc_test_protocol.Heartbeat_state_rpc.client ~version:versions.client
    in
    let%bind.Deferred.Or_error (), updates, metadata =
      Rpc.State_rpc.dispatch rpc (Command_rpc.Connection.rpc_connection conn) n
      >>| Or_error.join
    in
    let%bind updates = Pipe.to_list updates in
    match%map Rpc.State_rpc.close_reason metadata with
    | Closed_locally -> assert false
    | Closed_remotely -> Ok updates
    | Error e -> Error e
  in
  let%bind result =
    Command_rpc.Connection.with_close
      get_all_responses
      ~new_fds_for_rpc
      ~wait_for_stderr_transfer:false
      ~prog:"../bin/main.exe"
      ~args:
        ([ [ "state-conv" ]
         ; [ "-min-version"; Int.to_string versions.server_min ]
         ; [ "-max-version"; Int.to_string versions.server_max ]
         ]
         |> List.concat)
  in
  show_raise ~hide_positions:true (fun () ->
    [%test_eq: unit list] (ok_exn result) (List.init n ~f:ignore));
  Deferred.unit
;;

let%expect_test "client is up to date" =
  let%bind () = test false { client = 2; server_min = 1; server_max = 2 } in
  let%bind () = test true { client = 2; server_min = 1; server_max = 2 } in
  [%expect
    {|
    "did not raise"
    "did not raise"
    |}];
  return ()
;;

let%expect_test "client is acceptably behind" =
  let%bind () = test false { client = 1; server_min = 1; server_max = 2 } in
  let%bind () = test true { client = 1; server_min = 1; server_max = 2 } in
  [%expect
    {|
    "did not raise"
    "did not raise"
    |}];
  return ()
;;

let%expect_test "client is too far behind" =
  let%bind () = test false { client = 0; server_min = 1; server_max = 2 } in
  let%bind () = test true { client = 0; server_min = 1; server_max = 2 } in
  [%expect
    {|
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-state (Version 0)))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    heartbeat-state)
      (rpc_version 0)))
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-state (Version 0)))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    heartbeat-state)
      (rpc_version 0)))
    |}];
  return ()
;;

let%expect_test "client is ahead" =
  let%bind () = test false { client = 3; server_min = 1; server_max = 2 } in
  let%bind () = test true { client = 3; server_min = 1; server_max = 2 } in
  [%expect
    {|
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-state (Version 3)))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    heartbeat-state)
      (rpc_version 3)))
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-state (Version 3)))
      (connection_description (
        "Command_rpc client (parent process)"
        (prog      <hidden_in_test>)
        (args      <hidden_in_test>)
        (child_pid <hidden_in_test>)))
      (rpc_name    heartbeat-state)
      (rpc_version 3)))
    |}];
  return ()
;;
