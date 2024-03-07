open! Core
open Async

type query = unit [@@deriving bin_io, of_sexp]
type response = Nothing.t [@@deriving bin_io, sexp_of]

let rpc =
  Rpc.Rpc.create
    ~name:"exit_zero"
    ~version:1
    ~bin_query
    ~bin_response
    ~include_in_error_count:Only_on_exn
;;

let implementation _invocation () = Stdlib.exit 0
