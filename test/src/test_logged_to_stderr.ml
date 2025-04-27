open! Core
open! Async
open! Import

let _ = Command_rpc_test_protocol.Rpc_logged_to_stderr.rpc_one_way

let test stderr_handling new_fds_for_rpc n =
  let dispatch_rpc conn =
    let rpc = Command_rpc_test_protocol.Rpc_logged_to_stderr.rpc in
    Rpc.Rpc.dispatch rpc (Command_rpc.Connection.rpc_connection conn) n
  in
  Command_rpc.Connection.with_close
    dispatch_rpc
    ~new_fds_for_rpc
    ~wait_for_stderr_transfer:true
    ~stderr_handling
    ~prog:"../bin/main.exe"
    ~args:([ [ "logged-to-stderr" ] ] |> List.concat)
  |> Deferred.map ~f:ok_exn
;;

let%expect_test "propagate stderr" =
  let test = test Propagate_stderr in
  let%bind () = test false 0 in
  [%expect {| 0 |}];
  let%bind () = test false 5 in
  [%expect {| 5 |}];
  let%bind () = test false 100 in
  [%expect {| 100 |}];
  let%bind () = test true 0 in
  [%expect {| 0 |}];
  let%bind () = test true 5 in
  [%expect {| 5 |}];
  let%bind () = test true 100 in
  [%expect {| 100 |}];
  return ()
;;

let%expect_test "transfer to pipe" =
  let test n =
    let%map results =
      Deferred.List.map ~how:`Sequential [%all: bool] ~f:(fun new_fds_for_rpc ->
        let result = Set_once.create () in
        let f reader = Reader.contents reader >>| Set_once.set_exn result ~here:[%here] in
        let%map () = test (Custom f) new_fds_for_rpc n in
        [%sexp { new_fds_for_rpc : bool }], Set_once.get_exn result)
    in
    let result_string =
      List.all_equal results ~equal:[%equal: _ * string] |> Option.map ~f:snd
    in
    Expect_test_helpers_core.require
      ~if_false_then_print_s:
        (lazy
          [%sexp
            "Differing results based on function inputs. All should be the same:"
            , (results : (Sexp.t * string) list)])
      (Option.is_some result_string);
    Option.iter result_string ~f:print_endline
  in
  let%bind () = test 0 in
  [%expect {| 0 |}];
  let%bind () = test 5 in
  [%expect {| 5 |}];
  let%bind () = test 100 in
  [%expect {| 100 |}];
  return ()
;;
