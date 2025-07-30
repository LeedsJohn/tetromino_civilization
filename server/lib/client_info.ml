open! Core
open Tetromino_civilization_common

let moves_per_second = 10
let max_allowed_moves = moves_per_second * 60

type t =
  { mutable allowed_moves : int
  ; mutable previous_move_time : Time_ns.t
  ; bag : Tetromino_bag.t
  }

let create () =
  { allowed_moves = max_allowed_moves
  ; previous_move_time = Time_ns.now ()
  ; bag = Tetromino_bag.create ()
  }
;;

let is_allowed_to_move t = t.allowed_moves > 0
let previous_move_time t = t.previous_move_time
let is_allowed_to_spawn t piece_type = Tetromino_bag.mem t.bag piece_type

let made_move t =
  let cur_time = Time_ns.now () in
  t.previous_move_time <- cur_time;
  let get_second_count time =
    Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_sec
  in
  let new_allowed_moves =
    get_second_count cur_time - get_second_count t.previous_move_time
  in
  t.allowed_moves <- Int.max 0 (t.allowed_moves - 1);
  t.allowed_moves <- Int.min max_allowed_moves (t.allowed_moves + new_allowed_moves)
;;

let spawned_piece t piece_type =
  Tetromino_bag.remove t.bag piece_type;
  made_move t
;;
