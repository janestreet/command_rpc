open Core.Std
open Async.Std

module Command = struct
  module Invocation = struct
    type t = Sexp | Bin_io of Rpc.Connection.t
  end

  module type T = sig
    type query    [@@deriving of_sexp]
    type response [@@deriving sexp_of]
    val rpc : (query, response) Rpc.Rpc.t
    val implementation : Invocation.t -> query -> response Deferred.t
  end

  module type T_conv = sig
    include Versioned_rpc.Callee_converts.Rpc.S
    val name : string
    val query_of_sexp    : Sexp.t -> query
    val sexp_of_response : response -> Sexp.t
    val implementation : Invocation.t -> query -> response Deferred.t
  end

  module type T_pipe = sig
    type query    [@@deriving of_sexp]
    type response [@@deriving sexp_of]
    type error    [@@deriving sexp_of]
    val rpc : (query, response, error) Rpc.Pipe_rpc.t
    val implementation
      :  Invocation.t
      -> query
      -> (response Pipe.Reader.t, error) Result.t Deferred.t
  end

  type t = [
    | `Plain      of (module T)
    | `Plain_conv of (module T_conv)
    | `Pipe       of (module T_pipe)
  ]

  let menu impls =
    match
      Map.Poly.of_alist
        (List.concat_map impls ~f:(fun impl ->
          match impl with
          | `Plain plain ->
            let module T = (val plain : T) in
            [((Rpc.Rpc.name T.rpc, Rpc.Rpc.version T.rpc), impl)]
          | `Plain_conv x ->
            let module T = (val x : T_conv) in
            let versions = Set.to_list @@ T.versions () in
            List.map versions ~f:(fun version -> ((T.name, version), impl))
          | `Pipe pipe ->
            let module T = (val pipe : T_pipe) in
            [((Rpc.Pipe_rpc.name T.rpc, Rpc.Pipe_rpc.version T.rpc), impl)]
         ))
    with
    | `Ok map -> map
    | `Duplicate_key (name, version) ->
      failwithf "multiple implementations of rpc (%s %d)" name version ()

  let implementations ?log_not_previously_seen_version
    : t -> Invocation.t Rpc.Implementation.t list
    = function
      | `Plain (module T) ->
        [Rpc.Rpc.implement T.rpc T.implementation]
      | `Plain_conv (module T) ->
        T.implement_multi ?log_not_previously_seen_version
          (fun s ~version:_ q -> T.implementation s q)
      | `Pipe (module T) ->
        [Rpc.Pipe_rpc.implement T.rpc T.implementation]

  type call = {
    rpc_name : string;
    version : int;
    query : Sexp.t;
  } [@@deriving sexp]

  let write_sexp w sexp = Writer.write_sexp w sexp; Writer.newline w

  let main ?log_not_previously_seen_version impls ~show_menu mode =
    let stdout = Lazy.force Writer.stdout in
    if show_menu then
      let menu_sexp =
        [%sexp_of: (string * int) list] (Map.keys (menu impls))
      in
      write_sexp stdout menu_sexp;
      return `Success
    else
      let stdin = Lazy.force Reader.stdin in
      match mode with
      | `Bin_prot ->
        begin
          match
            Rpc.Implementations.create
              ~on_unknown_rpc:`Raise
              ~implementations:
                (Versioned_rpc.Menu.add
                   (List.concat_map ~f:(implementations ?log_not_previously_seen_version)
                      impls))
          with
          | Error (`Duplicate_implementations _) -> return `Failure
          | Ok implementations ->
            Rpc.Connection.server_with_close stdin stdout
              ~implementations ~connection_state:(fun conn -> Bin_io conn)
              ~on_handshake_error:`Raise
            >>| fun () ->
            `Success
        end
      | `Sexp ->
        Reader.read_sexp stdin
        >>= function
        | `Eof -> failwith "unexpected EOF on stdin"
        | `Ok sexp ->
          let call = call_of_sexp sexp in
          match Map.find (menu impls) (call.rpc_name, call.version) with
          | None -> failwithf "unimplemented rpc: (%s, %d)" call.rpc_name call.version ()
          | Some impl ->
            match impl with
            | `Plain (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>| fun response ->
              write_sexp stdout (T.sexp_of_response response);
              `Success
            | `Plain_conv (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>| fun response ->
              write_sexp stdout (T.sexp_of_response response);
              `Success
            | `Pipe (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>= function
              | Error e ->
                write_sexp stdout (T.sexp_of_error e);
                return `Failure
              | Ok pipe ->
                Pipe.iter pipe ~f:(fun r ->
                  write_sexp stdout (T.sexp_of_response r);
                  Deferred.unit)
                >>| fun () ->
                `Success

  let async_main status_deferred =
    upon status_deferred (fun status ->
      Shutdown.shutdown begin
        match status with
        | `Success -> 0
        | `Failure -> 1
      end);
    never_returns (Scheduler.go ())

  let menu_doc = " dump a sexp representation of the rpc menu"
  let sexp_doc = " speak sexp instead of bin-prot"

  let create ?log_not_previously_seen_version ~summary impls =
    Command.basic ~summary
      Command.Spec.(
        empty
        +> flag "-menu" no_arg ~doc:menu_doc
        +> flag "-sexp" no_arg ~doc:sexp_doc)
      (fun show_menu sexp () ->
         async_main
           (main ?log_not_previously_seen_version impls ~show_menu
              (if sexp then `Sexp else `Bin_prot)))

end

module Connection = struct
  type 'a with_connection_args
    =  ?propagate_stderr : bool        (* defaults to true *)
    -> ?env              : Process.env (* defaults to [`Extend []] *)
    -> prog              : string
    -> args              : string list
    -> 'a

  let transfer_stderr child_stderr =
    Reader.transfer child_stderr (Writer.pipe (Lazy.force Writer.stderr))
    >>= fun () ->
    Reader.close child_stderr

  let validate_program_name prog =
    let fail message =
      Deferred.Or_error.error "Command_rpc.Connection.connect" () (fun () ->
        [%sexp { reason = (message : string) ; filename = (prog : string) }])
    in
    Sys.file_exists prog
    >>= function
    | `No | `Unknown -> fail "file does not exist"
    | `Yes           ->
      Unix.stat prog
      >>= fun stat ->
      if stat.perm land 0o111 = 0
      then fail "file is not executable"
      else Deferred.Or_error.ok_unit

  let connect_gen ~propagate_stderr ~env ~prog ~args f =
    validate_program_name prog
    >>=? fun () ->
    Process.create ~prog ~args ~env ()
    >>=? fun process ->
    let stdin  = Process.stdin  process in
    let stdout = Process.stdout process in
    let stderr = Process.stderr process in
    don't_wait_for begin
      if propagate_stderr
      then transfer_stderr stderr
      else Reader.drain stderr
    end;
    (* This is mainly so that when a user closes the connection (which closes stdin and
       stdout) we will also close stderr. *)
    don't_wait_for begin
      Writer.close_finished stdin
      >>= fun () ->
      Reader.close_finished stdout
      >>= fun () ->
      Reader.close stderr
    end;
    let wait = Process.wait process in
    f ~stdin ~stdout ~wait
  ;;

  let with_close ?(propagate_stderr=true) ?(env=`Extend []) ~prog ~args dispatch_queries =
    connect_gen ~propagate_stderr ~env ~prog ~args
      (fun ~stdin ~stdout ~wait ->
         let%bind result =
           Rpc.Connection.with_close
             stdout stdin
             ~connection_state:(fun _ -> ())
             ~on_handshake_error:(`Call (fun exn -> return (Or_error.of_exn exn)))
             ~dispatch_queries
         in
         let%bind exit_or_signal = wait in
         ignore (exit_or_signal : Unix.Exit_or_signal.t);
         return result)
  ;;

  let create ?(propagate_stderr = true) ?(env=`Extend []) ~prog ~args () =
    connect_gen ~propagate_stderr ~env ~prog ~args
      (fun ~stdin ~stdout ~wait ->
         don't_wait_for (Deferred.ignore (wait : Unix.Exit_or_signal.t Deferred.t));
         Rpc.Connection.create
           stdout stdin
           ~connection_state:(fun _ -> ())
         >>| Or_error.of_exn_result)
  ;;
end
