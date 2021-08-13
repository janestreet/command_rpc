open! Core
open! Async

module type S =
  Command_rpc.Command.T_pipe_conv
  with type query = int
   and type response = int
   and type error = Error.t

module type Heartbeat_pipe_rpc = sig
  module type S = S

  val server : min_version:int -> max_version:int -> (module S)
  val client : version:int -> (int, int, Error.t) Rpc.Pipe_rpc.t
end
