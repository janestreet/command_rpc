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
  val implementation : state -> query -> response Deferred.t
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
    -> query
    -> (response Pipe.Reader.t, error) Result.t Deferred.t
end

module type Command_rpc = sig
  (** [Command] is used for setting up an RPC server in the child process.  By default this
      will set up an RPC server, but passing the [-sexp] flag will make it run the
      implementation on a sexp read from stdin instead.  Passing the [-menu] flag
      will cause the command to print out a sexp indicating which RPC names and
      versions are supported.
  *)
  module Command : sig
    module Invocation : sig
      type t =
        | Sexp
        | Bin_io of Rpc.Connection.t
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
          tell it how to extract an ['a] from it. Note that this extraction is done on every
          RPC call, so should be cheap and should not copy mutable state that you want to
          persist across calls. *)
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
      :  ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
      -> ?max_message_size:int
      -> ?log_not_previously_seen_version:(name:string -> int -> unit)
      -> ?buffer_age_limit:Writer.buffer_age_limit
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
          responding to them properly, but we cannot check that you don't do this or prevent
          you from doing it, so you just have to be careful.

          You are responsible for ensuring that the async scheduler is started, e.g., by
          calling [Command.async_or_error']. *)
      val param
        :  unit
        -> (?heartbeat_config:Rpc.Connection.Heartbeat_config.t
            -> ?max_message_size:int
            -> ?log_not_previously_seen_version:(name:string -> int -> unit)
            -> ?buffer_age_limit:Writer.buffer_age_limit
            (** Set the buffer age limit of the stdout writer *)
            -> t list
            -> unit Deferred.t)
             Command.Param.t
    end
  end

  module Connection : sig
    type t

    type 'a with_connection_args =
      ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
      -> ?max_message_size:int
      -> ?propagate_stderr:bool (* defaults to true *)
      -> ?env:Process.env (* defaults to [`Extend []] *)
      -> ?process_create:(prog:string
                          -> args:string list
                          -> ?env:Process.env
                          -> ?working_dir:string
                          -> unit
                          -> Process.t Deferred.Or_error.t)
      -> ?working_dir:string
      (* defaults to [Process.create]. You may want to use [process_create] to run
         Command_rpc on binaries from Exe_server. *)
      -> prog:string
      -> args:string list
      -> 'a

    (** [create] spawns a child process and returns an RPC connection that operates on the
        child's stdin and stdout. The child will be killed and reaped when the connection
        is closed. If [propagate_stderr] is true, the child's stderr will be printed on
        the parent's stderr; otherwise it will be ignored. *)
    val create : (unit -> t Or_error.t Deferred.t) with_connection_args

    (** [with_close] spawns a child and connects like [create], calls the function passed
        in on the resulting connection, and then closes the connection and kills the
        child. *)
    val with_close
      : ((t -> 'a Or_error.t Deferred.t) -> 'a Or_error.t Deferred.t)
          with_connection_args

    (** Get the RPC connection needed to talk to the command-rpc executable. *)
    val rpc_connection : t -> Rpc.Connection.t

    (** This module exists for testing purposes only. For example, clients can test
        whether their command-rpc server cleans up after itself properly when a ctrl-c
        at the command line kills a whole process group, server included. *)
    module Expert : sig
      (** Send a signal to the command-rpc executable.

          Note that this has a (very small) race condition where we can send a signal
          to a reaped pid if the process dies at exactly the right time. *)
      val kill : t -> Signal.t -> unit

      (** Wait for termination of the command-rpc executable. *)
      val wait : t -> Unix.Exit_or_signal.t Deferred.t
    end
  end
end
