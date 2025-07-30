open! Core
open! Async
open! Tetromino_civilization_client
module Protocol = Tetromino_civilization_protocol

let run () =
  let%bind conn =
    Rpc_websocket.Rpc.client (Uri.make ~host:"Johns-Mac-mini.local" ~port:31415 ())
  in
  let conn = Or_error.ok_exn conn in
  don't_wait_for (bruh conn);
  return ()
;;

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())
;;
