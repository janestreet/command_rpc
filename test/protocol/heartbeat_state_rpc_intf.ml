open! Core
open! Async
open! Import

module type S =
  Command_rpc.Command.T_state_conv_bin_io_only
  with type query = int
   and type initial_state = unit
   and type update = unit
   and type error = Error.t

module type Heartbeat_state_rpc = sig
  module type S = S

  val server : min_version:int -> max_version:int -> (module S)
  val client : version:int -> (int, unit, unit, Error.t) Rpc.State_rpc.t
end
