(** Utilities for RPC communication with a child process over stdin and stdout. *)

open! Core
open! Async

module type T = sig
  type query [@@deriving of_sexp]
  type response [@@deriving sexp_of]
  type state

  val rpc : (query, response) Rpc.Rpc.t
  val implementation : state -> query -> response Deferred.t
end

module type T_conv = sig
  include Versioned_rpc.Callee_converts.Rpc.S

  type state

  val name : string
  val query_of_sexp : Sexp.t -> query
  val sexp_of_response : response -> Sexp.t
  val implementation : state -> version:int -> query -> response Deferred.t
end

module type T_pipe = sig
  type query [@@deriving of_sexp]
  type response [@@deriving sexp_of]
  type error [@@deriving sexp_of]
  type state

  val rpc : (query, response, error) Rpc.Pipe_rpc.t

  val implementation
    :  state
    -> query
    -> (response Pipe.Reader.t, error) Result.t Deferred.t
end

module type T_pipe_conv = sig
  type query [@@deriving of_sexp]
  type response [@@deriving sexp_of]
  type error [@@deriving sexp_of]
  type state

  include
    Versioned_rpc.Callee_converts.Pipe_rpc.S
    with type query := query
    with type response := response
    with type error := error

  val implementation
    :  state
    -> version:int
    -> query
    -> (response Pipe.Reader.t, error) Result.t Deferred.t
end

module type Command_rpc = sig
  (** [Command] is used for setting up an RPC server in the child process communicating
      with the parent over stdin&stdout. By default this will use an Async RPC protocol,
      but passing the [-sexp] flag will make it use a sexp-based interface. Passing the
      [-menu] flag will cause the command to print out a sexp indicating which RPC names
      and versions are supported.

      Since stdout is used for communication with the parent process, it's not available
      for use as a debug output channel. We remap the file descriptors in the child in
      such a way that any attempted write to stdout gets sent to stderr instead. Then
      stderr can be forwarded to stderr of the parent if [propagate_stderr = true]. *)
  module Command : sig
    module Invocation : sig
      type t =
        | Sexp
        | Bin_io of Rpc.Connection.t
      [@@deriving sexp_of]
    end

    module Stateful : sig
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

      (** Given an RPC that expects a state type ['a], it can use a state type ['b] if we
          tell it how to extract an ['a] from it. Note that this extraction is done on
          every RPC call, so should be cheap and should not copy mutable state that you
          want to persist across calls. *)
      val lift : 'a t -> f:('b -> 'a) -> 'b t
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

    (** You need to call this on your list of stateful RPCs before they can be passed to
        [create] or (more usually) the function you get in [Expert.param]. *)
    val stateful : Invocation.t Stateful.t list -> t list

    val create
      :  ?connection_description:Info.t
      -> ?handshake_timeout:Time_float.Span.t
      -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
      -> ?heartbeat_timeout_style:Rpc.Connection.Heartbeat_timeout_style.t
      -> ?provide_rpc_shapes:bool
      -> ?max_message_size:int
      -> ?log_not_previously_seen_version:(name:string -> int -> unit)
      -> ?buffer_age_limit:Writer.buffer_age_limit
      -> ?on_connection:(Rpc.Connection.t -> unit)
      -> summary:string
      -> t list
      -> Command.t

    module Expert : sig
      (** [param ()] returns a command line parameter which produces a function. You can
          do any initialization (e.g. of mutable state) and then call the function with
          your RPC implementations to start the RPC server. The deferred it returns will
          become determined when the client closes their connection, after which you may
          do any cleanup you need and then exit (possibly with an appropriate exit
          status).

          This interface is marked [Expert] because consuming from stdin or writing to
          stdout during your initialization may prevent you from receiving RPCs or
          responding to them properly, but we cannot check that you don't do this or
          prevent you from doing it, so you just have to be careful.

          You are responsible for ensuring that the async scheduler is started, e.g., by
          calling [Command.async_or_error']. *)
      val param
        :  unit
        -> (?connection_description:Info.t
            -> ?handshake_timeout:Time_float.Span.t
            -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
            -> ?heartbeat_timeout_style:Rpc.Connection.Heartbeat_timeout_style.t
            -> ?provide_rpc_shapes:bool
            -> ?max_message_size:int
            -> ?log_not_previously_seen_version:(name:string -> int -> unit)
            -> ?buffer_age_limit:Writer.buffer_age_limit
                 (** Set the buffer age limit of the stdout writer *)
            -> ?on_connection:(Rpc.Connection.t -> unit)
            -> t list
            -> unit Deferred.t)
             Command.Param.t
    end
  end

  module Connection : sig
    module Stdout_handling : sig
      type t =
        | Default
        (** semantics depend on [new_fds_for_rpc]. If [new_fds_for_rpc = false], [stdout]
            is merged with [stderr], and then handled however [stderr] is handled (see
            [Stderr_handling]). If [new_fds_for_rpc = true], then [stdout] is propagated. *)
        | Propagate_stdout
        | Custom of (Reader.t -> unit Deferred.t)

      (** [Default], semantics depend on [new_fds_for_rpc] *)
      val default : t
    end

    module Stderr_handling : sig
      type t =
        | Propagate_stderr
        | Ignore_stderr
        | Custom of (Reader.t -> unit Deferred.t)

      (** [Propagate_stderr] *)
      val default : t
    end

    type t

    type 'a with_connection_args =
      ?new_fds_for_rpc:bool
        (** Defaults to [false]. If [true], instead of using [stdin] and [stdout] for the
            RPC connection, new pipes will be created for the connection, and [stdout]
            from the child process won't be merged with [stderr]. *)
      -> ?stdout_handling:Stdout_handling.t (** default: [Stdout_handling.default] *)
      -> ?stderr_handling:Stderr_handling.t (** default: [Stderr_handling.default] *)
      -> ?wait_for_stderr_transfer:bool
           (** Defaults to [true]. If set to true, makes [with_close] and [Expert.wait]
               wait for stderr to have been fully propagated, fully drained, or for the
               user callback (in the case of [stderr_handling = Custom _]) to complete. *)
      -> ?connection_description:Info.t
      -> ?handshake_timeout:Time_float.Span.t
      -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
      -> ?heartbeat_timeout_style:Rpc.Connection.Heartbeat_timeout_style.t
      -> ?provide_rpc_shapes:bool
      -> ?max_message_size:int
      -> ?buffer_age_limit:Writer.buffer_age_limit
      -> ?implementations:unit Rpc.Implementations.t
      -> ?env:Process.env (* defaults to [`Extend []] *)
      -> ?process_create:
           (prog:string
            -> args:string list
            -> ?env:Process.env
            -> ?working_dir:string
            -> unit
            -> Process.t Deferred.Or_error.t)
      -> (* [process_create] defaults to [Process.create]. You may want to use it to run
            Command_rpc on binaries from Exe_server. *)
         ?working_dir:string
      -> prog:string
      -> args:string list
      -> 'a

    (** [create] spawns a child process and returns an RPC connection that operates on the
        child's stdin and stdout. The child will be killed and reaped when the connection
        is closed. If [propagate_stderr] is true, the child's stderr will be printed on
        the parent's stderr; otherwise it will be ignored. *)
    val create : (unit -> t Or_error.t Deferred.t) with_connection_args

    (** [with_close] spawns a child and connects like [create], then calls the function
        passed in on the resulting connection. Like [Rpc.Connection.with_close], if
        [implementations] is passed, [with_close] will not return until the connection is
        closed by either the parent or the child; otherwise, it closes the connection and
        kills the child when the provided function returns. *)
    val with_close
      : ((t -> 'a Or_error.t Deferred.t) -> 'a Or_error.t Deferred.t) with_connection_args

    (** Get the RPC connection needed to talk to the command-rpc executable. *)
    val rpc_connection : t -> Rpc.Connection.t

    (** This module contains some functions that let you interact with the underlying
        child process. There's nothing particularly tricky about them, but most users
        don't need them. *)
    module Expert : sig
      (** Get the underlying command-rpc worker process. Can be used to send SIGKILL
          signals for example. *)
      val process : t -> Process.t

      (** Wait for termination of the command-rpc executable and return the exit status.
          This can be used e.g. after [with_close] to collect the missing info, since
          [with_close] does not report it. *)
      val wait : t -> Unix.Exit_or_signal.t Deferred.t
    end
  end
end
