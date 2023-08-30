open! Core
open! Async

let caller_implementations_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param () in
     fun () ->
       serve
         [ `Plain (module Command_rpc_test_protocol.Caller_implementations_rpcs.Callee) ])
    ~behave_nicely_in_pipeline:false
;;

let pipe_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Pipe_conv
             (Command_rpc_test_protocol.Heartbeat_pipe_rpc.server
                ~min_version
                ~max_version
               :> (module Command_rpc.Command.T_pipe_conv))
         ])
    ~behave_nicely_in_pipeline:false
;;

let exit_zero_rpc_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param () in
     fun () -> serve [ `Plain (module Command_rpc_test_protocol.Exit_zero_rpc) ])
    ~behave_nicely_in_pipeline:false
;;

let pipe_direct_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param () in
     fun () ->
       serve
         [ `Implementations
             Command_rpc_test_protocol.Heartbeat_pipe_direct_rpc.implementations
         ])
    ~behave_nicely_in_pipeline:false
;;

let logged_to_stderr_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param () in
     fun () ->
       serve
         [ `Implementations Command_rpc_test_protocol.Rpc_logged_to_stderr.implementations
         ])
    ~behave_nicely_in_pipeline:false
;;

let state_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Implementations
             (Command_rpc_test_protocol.Heartbeat_state_rpc.server
                ~min_version
                ~max_version)
         ])
    ~behave_nicely_in_pipeline:false
;;

let streamable_state_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Implementations
             (Command_rpc_test_protocol.Heartbeat_streamable_state_rpc.server
                ~min_version
                ~max_version)
         ])
    ~behave_nicely_in_pipeline:false
;;

let () =
  Command.group
    ~summary:""
    [ "caller-implementations", caller_implementations_command
    ; "exit-zero-rpc", exit_zero_rpc_command
    ; "pipe-conv", pipe_conv_command
    ; "pipe-direct", pipe_direct_command
    ; "logged-to-stderr", logged_to_stderr_command
    ; "state-conv", state_conv_command
    ; "streamable-state-conv", streamable_state_conv_command
    ]
  |> Command_unix.run
;;
