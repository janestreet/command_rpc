open! Core
open! Async

include
  Command_rpc.Command.T_state
  with type query = int
   and type initial_state = string
   and type update = int
   and type error = Error.t
