open! Core
open! Async
open! Import

module Caller : sig
  val rpc : (unit, int) Rpc.Rpc.t
  val implementations : unit Rpc.Implementations.t
end = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"caller_rpc"
      ~version:1
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: int]
      ~include_in_error_count:Only_on_exn
  ;;

  let implementation () () = return 42

  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Raise
      ~implementations:[ Rpc.Rpc.implement rpc implementation ]
      ~on_exception:Log_on_background_exn
  ;;
end

module Callee :
  Command_rpc.Command.T with type query = unit and type response = string Or_error.t =
struct
  type query = unit [@@deriving bin_io, of_sexp]
  type response = string Or_error.t [@@deriving bin_io, sexp_of]

  let rpc =
    Rpc.Rpc.create
      ~name:"callee_rpc"
      ~version:1
      ~bin_query
      ~bin_response
      ~include_in_error_count:Or_error
  ;;

  let implementation invocation () =
    match (invocation : Command_rpc.Command.Invocation.t) with
    | Sexp ->
      Deferred.Or_error.error_string
        "I can't know what the secret number is because I was invoked in sexp mode"
    | Bin_io connection ->
      let%map.Deferred.Or_error secret_number =
        Rpc.Rpc.dispatch Caller.rpc connection ()
      in
      [%string "The secret number is %{secret_number#Int}"]
  ;;
end
