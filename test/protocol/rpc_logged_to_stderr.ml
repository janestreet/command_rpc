open! Core
open! Async
open! Import

let rpc =
  let open Core.Core_stable in
  Rpc.Rpc.create
    ~name:"rpc-logged-to-stderr"
    ~version:1
    ~bin_query:Int.V1.bin_t
    ~bin_response:Unit.V1.bin_t
    ~include_in_error_count:Only_on_exn
;;

let rpc_one_way =
  let open Core.Core_stable in
  Rpc.One_way.create ~name:"rpc-logged-to-stderr-one-way" ~version:1 ~bin_msg:Int.V1.bin_t
;;

let implementation_one_way (_ : Command_rpc.Command.Invocation.t) n = eprintf "%d\n" n

let implementation (_ : Command_rpc.Command.Invocation.t) n =
  eprintf "%d\n" n;
  Deferred.return ()
;;

let implementations =
  [ Rpc.One_way.implement
      rpc_one_way
      implementation_one_way
      ~on_exception:Close_connection
  ; Rpc.Rpc.implement rpc implementation
  ]
;;
