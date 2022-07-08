open! Core
open! Async
open! Import

let test new_fds_for_rpc num_heartbeats =
  let count_heartbeats conn =
    let open Deferred.Or_error.Let_syntax in
    let rpc = Command_rpc_test_protocol.Heartbeat_pipe_direct_rpc.rpc in
    let%bind responses, _metadata =
      Rpc.Pipe_rpc.dispatch
        rpc
        (Command_rpc.Connection.rpc_connection conn)
        num_heartbeats
      |> Deferred.map ~f:Or_error.join
    in
    let%map num_responses = Pipe.to_list responses |> Deferred.ok >>| List.length in
    print_s [%sexp (num_responses : int)]
  in
  Command_rpc.Connection.with_close
    count_heartbeats
    ~new_fds_for_rpc
    ~wait_for_stderr_transfer:false
    ~prog:"../bin/main.exe"
    ~args:([ [ "pipe-direct" ] ] |> List.concat)
  |> Deferred.map ~f:ok_exn
;;

let%expect_test _ =
  let%bind () = test false 0 in
  [%expect {| 0 |}];
  let%bind () = test false 5 in
  [%expect {| 5 |}];
  let%bind () = test false 100 in
  [%expect {| 100 |}];
  let%bind () = test true 0 in
  [%expect {| 0 |}];
  let%bind () = test true 5 in
  [%expect {| 5 |}];
  let%bind () = test true 100 in
  [%expect {| 100 |}];
  return ()
;;
