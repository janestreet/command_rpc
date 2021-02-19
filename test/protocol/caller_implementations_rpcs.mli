open! Core
open! Async
open! Import

module Caller : sig
  val rpc : (unit, int) Rpc.Rpc.t
  val implementations : unit Rpc.Implementations.t
end

module Callee :
  Command_rpc.Command.T with type query = unit and type response = string Or_error.t
