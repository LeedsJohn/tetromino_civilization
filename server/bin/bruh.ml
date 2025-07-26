open! Core
open! Async
open! Tetromino_civilization_server

let () =
  don't_wait_for (do_thing ());
  never_returns (Scheduler.go ())
;;
