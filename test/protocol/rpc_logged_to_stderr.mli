open! Core
open! Async

val rpc : (int, unit) Rpc.Rpc.t
val rpc_one_way : int Rpc.One_way.t
val implementations : Command_rpc.Command.Invocation.t Rpc.Implementation.t list
