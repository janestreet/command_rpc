open Core
open Poly
open Async
open Command_rpc_intf

module Default_timeouts = struct
  (* Here we greatly increase the default timeouts that we inherit from Async_rpc.
     The reason it makes sense to have larger defaults here are:
     - it's common to use [Command_rpc] to offload slow computations to the child process,
       so long async cycles in the child are more likely, which makes spurious timeout
       more likely;
     - command_rpc timeouts are almost always fatal for the application, while it's
       somewhat-common to have retries or fallbacks for network services;
     - command_rpc commands are often started from NFS, or compiled on the first start,
       and we've seen that take some tens of second occasionally.

     The reason the parent timeouts are shorter than child timeouts is that errors
     detected in the parent have a simpler error handling story.
  *)
  let default_handshake_timeout ~side =
    match side with
    | `parent -> Time.Span.of_min 10.
    | `child -> Time.Span.of_hr 1.
  ;;

  let default_heartbeat_config ~side =
    Rpc.Connection.Heartbeat_config.create
      ~timeout:
        (Time_ns.Span.of_span_float_round_nearest_microsecond
           (default_handshake_timeout ~side))
      ~send_every:(Time_ns.Span.of_sec 10.)
      ()
  ;;
end

open Default_timeouts

module Command = struct
  module Invocation = struct
    type t =
      | Sexp
      | Bin_io of Rpc.Connection.t
  end

  module Stateful = struct
    module type T = T
    module type T_conv = T_conv
    module type T_pipe = T_pipe
    module type T_pipe_conv = T_pipe_conv

    type 'state t =
      [ `Plain of (module T with type state = 'state)
      | `Plain_conv of (module T_conv with type state = 'state)
      | `Pipe of (module T_pipe with type state = 'state)
      | `Pipe_conv of (module T_pipe_conv with type state = 'state)
      | `Implementations of 'state Rpc.Implementation.t list
      ]

    let lift (type a b) (t : a t) ~(f : b -> a) : b t =
      match t with
      | `Plain (module M) ->
        `Plain
          (module struct
            include (M : T with type state := a)

            type state = b

            let implementation state query = implementation (f state) query
          end)
      | `Plain_conv (module M) ->
        `Plain_conv
          (module struct
            include (M : T_conv with type state := a)

            type state = b

            let implementation state ~version query =
              implementation (f state) ~version query
            ;;
          end)
      | `Pipe (module M) ->
        `Pipe
          (module struct
            include (M : T_pipe with type state := a)

            type state = b

            let implementation state query = implementation (f state) query
          end)
      | `Pipe_conv (module M) ->
        `Pipe_conv
          (module struct
            include (M : T_pipe_conv with type state := a)

            type state = b

            let implementation state ~version query =
              implementation (f state) ~version query
            ;;
          end)
      | `Implementations impls ->
        `Implementations (List.map impls ~f:(Rpc.Implementation.lift ~f))
    ;;
  end

  module type T = Stateful.T with type state := Invocation.t
  module type T_conv = Stateful.T_conv with type state := Invocation.t
  module type T_pipe = Stateful.T_pipe with type state := Invocation.t
  module type T_pipe_conv = Stateful.T_pipe_conv with type state := Invocation.t

  type t =
    [ `Plain of (module T)
    | `Plain_conv of (module T_conv)
    | `Pipe of (module T_pipe)
    | `Pipe_conv of (module T_pipe_conv)
    | `Implementations of Invocation.t Rpc.Implementation.t list
    ]

  let stateful (rpcs : Invocation.t Stateful.t list) = (rpcs :> t list)

  let menu impls =
    match
      Map.Poly.of_alist
        (List.concat_map impls ~f:(fun impl ->
           match impl with
           | `Plain plain ->
             let module T = (val plain : T) in
             [ (Rpc.Rpc.name T.rpc, Rpc.Rpc.version T.rpc), impl ]
           | `Plain_conv x ->
             let module T = (val x : T_conv) in
             let versions = Set.to_list @@ T.versions () in
             List.map versions ~f:(fun version -> (T.name, version), impl)
           | `Pipe pipe ->
             let module T = (val pipe : T_pipe) in
             [ (Rpc.Pipe_rpc.name T.rpc, Rpc.Pipe_rpc.version T.rpc), impl ]
           | `Pipe_conv pipe ->
             let module T = (val pipe : T_pipe_conv) in
             let versions = Set.to_list @@ T.versions () in
             List.map versions ~f:(fun version -> (T.name, version), impl)
           | `Implementations impls ->
             List.map impls ~f:(fun impl ->
               let desc = Rpc.Implementation.description impl in
               (desc.name, desc.version), `Implementations impls)))
    with
    | `Ok map -> map
    | `Duplicate_key (name, version) ->
      failwithf "multiple implementations of rpc (%s %d)" name version ()
  ;;

  let implementations ?log_not_previously_seen_version
    : t -> Invocation.t Rpc.Implementation.t list
    = function
      | `Plain (module T) -> [ Rpc.Rpc.implement T.rpc T.implementation ]
      | `Plain_conv (module T) ->
        T.implement_multi ?log_not_previously_seen_version (fun s ~version q ->
          T.implementation s ~version q)
      | `Pipe (module T) -> [ Rpc.Pipe_rpc.implement T.rpc T.implementation ]
      | `Pipe_conv (module T) ->
        T.implement_multi ?log_not_previously_seen_version (fun s ~version q ->
          T.implementation s ~version q)
      | `Implementations impls -> impls
  ;;

  type call =
    { rpc_name : string
    ; version : int
    ; query : Sexp.t
    }
  [@@deriving sexp]

  let write_sexp w sexp =
    Writer.write_sexp w sexp;
    Writer.newline w
  ;;

  (** This function returns [stdin] and [stdout] that are connected to the same kernel
      objects (files/pipes/etc) as [Reader.stdin] and [Writer.stdout] before the
      call, except they will have new file descriptor numbers (greater than 2) to avoid
      the other parts of the program writing there by accident.

      It also changes file descriptors such that after this function [Reader.stdin] is
      reading from /dev/null and [Writer.stdout] is writing to stderr.
  *)
  let claim_stdin_and_stdout_for_exclusive_use ?buffer_age_limit () =
    let same_fd fd1 fd2 =
      Int.( = ) (Core_unix.File_descr.to_int fd1) (Core_unix.File_descr.to_int fd2)
    in
    let equivalent_fd fd1 fd2 =
      (* this is the same check [Writer] does when sharing the writer between stderr
         and stdout *)
      let dev_and_ino fd =
        let stats = Core_unix.fstat fd in
        stats.st_dev, stats.st_ino
      in
      same_fd fd1 fd2 || dev_and_ino fd1 = dev_and_ino fd2
    in
    let stdin = Lazy.force Reader.stdin in
    let stdout = Lazy.force Writer.stdout in
    let stderr = Lazy.force Writer.stderr in
    assert (same_fd (Fd.file_descr_exn (Reader.fd stdin)) Core_unix.stdin);
    (* Async has a special hack where if file descriptors 1 and 2 happen to point to the
       same file/device (for example when running in a tty) then it only creates one
       writer (ignoring file descriptor 2) that's used for both [Writer.stderr] and
       [Writer.stdout]. In this situation [same_fd] will be false, but [equivalent_fd]
       will be true and that should be enough to keep sanity below. *)
    assert (equivalent_fd (Fd.file_descr_exn (Writer.fd stdout)) Core_unix.stdout);
    assert (equivalent_fd (Fd.file_descr_exn (Writer.fd stderr)) Core_unix.stderr);
    let make_a_copy_of_stdin_and_stdout () =
      let dupped_stdin = Core_unix.dup Core_unix.stdin in
      Core_unix.set_close_on_exec dupped_stdin;
      let dupped_stdout = Core_unix.dup Core_unix.stdout in
      Core_unix.set_close_on_exec dupped_stdout;
      assert (Core_unix.File_descr.to_int dupped_stdin > 2);
      assert (Core_unix.File_descr.to_int dupped_stdout > 2);
      let create_fd ~similar_to fd =
        Fd.create (Fd.kind similar_to) fd (Fd.info similar_to)
      in
      let stdin = Reader.create (create_fd ~similar_to:(Reader.fd stdin) dupped_stdin) in
      let stdout =
        Writer.create
          ?buffer_age_limit
          (create_fd ~similar_to:(Writer.fd stdout) dupped_stdout)
      in
      stdin, stdout
    in
    let make_sure_stdin_and_stdout_are_not_used () =
      (* After this, anyone attempting to read from stdin gets an empty result
         and anything written to stdout goes to stderr instead. *)
      let dev_null = Core_unix.openfile ~mode:[ O_RDONLY ] "/dev/null" in
      Async.Fd.expect_file_descr_redirection Core_unix.stdin ~f:(fun () ->
        Core_unix.dup2 ~src:dev_null ~dst:Core_unix.stdin ());
      Async.Fd.expect_file_descr_redirection Core_unix.stdout ~f:(fun () ->
        Core_unix.dup2 ~src:Core_unix.stderr ~dst:Core_unix.stdout ());
      Core_unix.close dev_null
    in
    let res = make_a_copy_of_stdin_and_stdout () in
    make_sure_stdin_and_stdout_are_not_used ();
    res
  ;;

  let main
        ?connection_description
        ?(handshake_timeout = default_handshake_timeout ~side:`child)
        ?(heartbeat_config = default_heartbeat_config ~side:`child)
        ?max_message_size
        ?log_not_previously_seen_version
        ?buffer_age_limit
        impls
        ~show_menu
        mode
    =
    if show_menu
    then (
      let menu_sexp = [%sexp_of: (string * int) list] (Map.keys (menu impls)) in
      write_sexp (Lazy.force Writer.stdout) menu_sexp;
      return `Success)
    else (
      let stdin, stdout = claim_stdin_and_stdout_for_exclusive_use ?buffer_age_limit () in
      match mode with
      | `Bin_prot ->
        (match
           Rpc.Implementations.create
             ~on_unknown_rpc:`Raise
             ~implementations:
               (Versioned_rpc.Menu.add
                  (List.concat_map
                     ~f:(implementations ?log_not_previously_seen_version)
                     impls))
         with
         | Error (`Duplicate_implementations descriptions) ->
           raise_s
             [%message "duplicate implementations" (descriptions : Rpc.Description.t list)]
         | Ok implementations ->
           Rpc.Connection.server_with_close
             stdin
             stdout
             ?description:connection_description
             ~handshake_timeout
             ~heartbeat_config
             ?max_message_size
             ~implementations
             ~connection_state:(fun conn -> Bin_io conn)
             ~on_handshake_error:`Raise
           >>| fun () -> `Success)
      | `Sexp ->
        Reader.read_sexp stdin
        >>= (function
          | `Eof -> failwith "unexpected EOF on stdin"
          | `Ok sexp ->
            let call = call_of_sexp sexp in
            (match Map.find (menu impls) (call.rpc_name, call.version) with
             | None -> failwithf "unimplemented rpc: (%s, %d)" call.rpc_name call.version ()
             | Some impl ->
               (match impl with
                | `Plain (module T) ->
                  let query = T.query_of_sexp call.query in
                  T.implementation Sexp query
                  >>| fun response ->
                  write_sexp stdout (T.sexp_of_response response);
                  `Success
                | `Plain_conv (module T) ->
                  let query = T.query_of_sexp call.query in
                  T.implementation Sexp ~version:call.version query
                  >>| fun response ->
                  write_sexp stdout (T.sexp_of_response response);
                  `Success
                | `Pipe (module T) ->
                  let query = T.query_of_sexp call.query in
                  T.implementation Sexp query
                  >>= (function
                    | Error e ->
                      write_sexp stdout (T.sexp_of_error e);
                      return `Failure
                    | Ok pipe ->
                      Pipe.iter pipe ~f:(fun r ->
                        write_sexp stdout (T.sexp_of_response r);
                        Deferred.unit)
                      >>| fun () -> `Success)
                | `Pipe_conv (module T) ->
                  let query = T.query_of_sexp call.query in
                  T.implementation Sexp ~version:call.version query
                  >>= (function
                    | Error e ->
                      write_sexp stdout (T.sexp_of_error e);
                      return `Failure
                    | Ok pipe ->
                      Pipe.iter pipe ~f:(fun r ->
                        write_sexp stdout (T.sexp_of_response r);
                        Deferred.unit)
                      >>| fun () -> `Success)
                | `Implementations _ ->
                  failwithf
                    "This RPC is not supported in [-sexp] mode: (%s, %d)"
                    call.rpc_name
                    call.version
                    ()))))
  ;;

  let async_main status_deferred =
    upon status_deferred (fun status ->
      Shutdown.shutdown
        (match status with
         | `Success -> 0
         | `Failure -> 1));
    never_returns (Scheduler.go ())
  ;;

  let menu_doc = " dump a sexp representation of the rpc menu"
  let sexp_doc = " speak sexp instead of bin-prot"

  module Expert = struct
    let param_exit_status () =
      let open Command.Let_syntax in
      [%map_open
        let show_menu = flag "-menu" no_arg ~doc:menu_doc
        and sexp = flag "-sexp" no_arg ~doc:sexp_doc in
        fun ?connection_description
          ?handshake_timeout
          ?heartbeat_config
          ?max_message_size
          ?log_not_previously_seen_version
          ?buffer_age_limit
          impls ->
          main
            ?connection_description
            ?handshake_timeout
            ?heartbeat_config
            ?max_message_size
            ?log_not_previously_seen_version
            ?buffer_age_limit
            impls
            ~show_menu
            (if sexp then `Sexp else `Bin_prot)]
    ;;

    let param () =
      Command.Param.map
        (param_exit_status ())
        ~f:(fun
             main
             ?connection_description
             ?handshake_timeout
             ?heartbeat_config
             ?max_message_size
             ?log_not_previously_seen_version
             ?buffer_age_limit
             rpcs
             ->
               (* If you want to detect success or failure and do something appropriate,
                  you can just do that from your RPC implementation. But we still need
                  [param_exit_status] separately because [create] below doesn't have
                  access to the RPC implementations. *)
               main
                 ?connection_description
                 ?handshake_timeout
                 ?heartbeat_config
                 ?max_message_size
                 ?log_not_previously_seen_version
                 ?buffer_age_limit
                 rpcs
               >>| function
               | `Success | `Failure -> ())
    ;;
  end

  let create
        ?connection_description
        ?handshake_timeout
        ?heartbeat_config
        ?max_message_size
        ?log_not_previously_seen_version
        ?buffer_age_limit
        ~summary
        impls
    =
    let open Command.Let_syntax in
    Command.basic
      ~summary
      [%map_open
        let main = Expert.param_exit_status () in
        fun () ->
          async_main
            (main
               ?connection_description
               ?handshake_timeout
               ?heartbeat_config
               ?max_message_size
               ?log_not_previously_seen_version
               ?buffer_age_limit
               impls)]
  ;;
end

module Connection = struct
  (* We explicitly [Process.wait] as soon as we start our connection. This guarantees the
     subprocess gets cleaned up even if the client does not explicitly wait. *)
  type t =
    { process : Process.t
    ; wait : Unix.Exit_or_signal.t Deferred.t
    ; rpc_connection : Rpc.Connection.t
    }

  let rpc_connection t = t.rpc_connection
  let wait t = t.wait
  let kill t signal = Process.send_signal t.process signal

  type 'a with_connection_args =
    ?wait_for_stderr_transfer:bool
    -> ?connection_description:Info.t
    -> ?handshake_timeout:Time.Span.t
    -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> ?max_message_size:int
    -> ?implementations:unit Rpc.Implementations.t
    -> ?propagate_stderr:bool (* defaults to true *)
    -> ?env:Process.env (* defaults to [`Extend []] *)
    -> ?process_create:
         (prog:string
          -> args:string list
          -> ?env:Process.env
          -> ?working_dir:string
          -> unit
          -> Process.t Deferred.Or_error.t)
    -> ?working_dir:string
    -> prog:string
    -> args:string list
    -> 'a

  let transfer_stderr child_stderr =
    Reader.transfer child_stderr (Writer.pipe (Lazy.force Writer.stderr))
    >>= fun () -> Reader.close child_stderr
  ;;

  let connect_gen
        ?(wait_for_stderr_transfer = false)
        ?(process_create =
          fun ~prog ~args ?env ?working_dir () ->
            Process.create ~prog ~args ?env ?working_dir ())
        ~propagate_stderr
        ~env
        ~prog
        ~args
        ?working_dir
        f
    =
    process_create ~prog ~args ~env ?working_dir ()
    >>=? fun process ->
    let stdin = Process.stdin process in
    let stdout = Process.stdout process in
    let stderr = Process.stderr process in
    let stderr_flushed =
      if propagate_stderr then transfer_stderr stderr else Reader.drain stderr
    in
    if not wait_for_stderr_transfer
    then
      (* This is mainly so that when a user closes the connection (which closes stdin and
         stdout) we will also close stderr. *)
      don't_wait_for
        (Writer.close_finished stdin
         >>= fun () -> Reader.close_finished stdout >>= fun () -> Reader.close stderr);
    let wait = Process.wait process in
    let wait =
      match wait_for_stderr_transfer with
      | false -> wait
      | true ->
        let%map wait = wait
        and () = stderr_flushed in
        wait
    in
    f ~process ~wait
  ;;

  let with_close
        ?wait_for_stderr_transfer
        ?connection_description
        ?handshake_timeout
        ?heartbeat_config
        ?max_message_size
        ?implementations
        ?(propagate_stderr = true)
        ?(env = `Extend [])
        ?process_create
        ?working_dir
        ~prog
        ~args
        dispatch_queries
    =
    connect_gen
      ?wait_for_stderr_transfer
      ?process_create
      ~propagate_stderr
      ~env
      ~prog
      ~args
      ?working_dir
      (fun ~process ~wait ->
         let%bind result =
           Rpc.Connection.with_close
             (Process.stdout process)
             (Process.stdin process)
             ?description:connection_description
             ?handshake_timeout
             ?heartbeat_config
             ?max_message_size
             ?implementations
             ~connection_state:(fun _ -> ())
             ~on_handshake_error:(`Call (fun exn -> return (Or_error.of_exn exn)))
             ~dispatch_queries:(fun rpc_connection ->
               dispatch_queries { process; wait; rpc_connection })
         in
         let%bind exit_or_signal = wait in
         ignore (exit_or_signal : Unix.Exit_or_signal.t);
         return result)
  ;;

  let create
        ?wait_for_stderr_transfer
        ?connection_description
        ?(handshake_timeout = default_handshake_timeout ~side:`parent)
        ?(heartbeat_config = default_heartbeat_config ~side:`parent)
        ?max_message_size
        ?implementations
        ?(propagate_stderr = true)
        ?(env = `Extend [])
        ?process_create
        ?working_dir
        ~prog
        ~args
        ()
    =
    connect_gen
      ?wait_for_stderr_transfer
      ?process_create
      ~propagate_stderr
      ~env
      ~prog
      ~args
      ?working_dir
      (fun ~process ~wait ->
         Rpc.Connection.create
           (Process.stdout process)
           (Process.stdin process)
           ?description:connection_description
           ~handshake_timeout
           ~heartbeat_config
           ?max_message_size
           ?implementations
           ~connection_state:(fun _ -> ())
         >>| Or_error.of_exn_result
         >>| Or_error.map ~f:(fun rpc_connection -> { process; wait; rpc_connection }))
  ;;

  module Expert = struct
    let wait = wait
    let kill = kill
  end
end
