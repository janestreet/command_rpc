open! Core
open! Async

type query = int [@@deriving of_sexp]
type initial_state = string [@@deriving sexp_of]
type update = int [@@deriving sexp_of]
type error = Error.t [@@deriving sexp_of]

let rpc =
  Rpc.State_rpc.create
    ~name:"simple-state"
    ~version:1
    ~bin_query:[%bin_type_class: int]
    ~bin_state:[%bin_type_class: string]
    ~bin_update:[%bin_type_class: int]
    ~bin_error:[%bin_type_class: Error.t]
    ()
;;

let implementation _invocation n =
  let initial_state = [%string "got %{n#Int} items"] in
  return (Ok (initial_state, Pipe.of_list (List.init n ~f:Fn.id)))
;;
