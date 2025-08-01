open! Core
open Tetromino_civilization_common

type t =
  { board : Board.t
  ; moves_by_chunk : Action_list.t array
  ; loaded_chunks : int list Hashtbl.M(Client_id).t
  ; last_sent_move : int Hashtbl.M(Client_id).t
  ; mutable move_count : int
  }

let create ~start_num_chunks ~max_num_chunks ~num_rows ~chunk_cols =
  let board = Board.create ~start_num_chunks ~max_num_chunks ~num_rows ~chunk_cols in
  { board
  ; moves_by_chunk = Array.init max_num_chunks ~f:(fun _i -> Action_list.make 100)
  ; loaded_chunks = Hashtbl.create (module Client_id)
  ; last_sent_move = Hashtbl.create (module Client_id)
  ; move_count = 0
  }
;;

let get_board t = t.board

let add_client t client_id =
  Hashtbl.set
    t.loaded_chunks
    ~key:client_id
    ~data:(List.range 0 (Array.length t.moves_by_chunk));
  Hashtbl.set t.last_sent_move ~key:client_id ~data:t.move_count
;;

let get_new_moves t client_id =
  let last_seen_move = Hashtbl.find_exn t.last_sent_move client_id in
  let compare_entry (mc1, _act1) (mc2, _act2) = Int.compare mc1 mc2 in
  let new_moves =
    Hashtbl.find_exn t.loaded_chunks client_id
    |> List.map ~f:(fun chunk_id ->
      Action_list.get_actions_after t.moves_by_chunk.(chunk_id) last_seen_move)
    |> List.join
    |> List.dedup_and_sort ~compare:compare_entry
  in
  (match List.max_elt new_moves ~compare:compare_entry with
   | None -> ()
   | Some (move_num, _action) ->
     Hashtbl.set t.last_sent_move ~key:client_id ~data:move_num);
  List.map new_moves ~f:snd
;;

let col_to_chunk_id t col = col / Board.cols_per_chunk t.board

(* [action] added to all [chunk_ids] with the same move number. *)
let add_action t chunk_ids action =
  let chunk_ids = List.stable_dedup chunk_ids ~compare:Int.compare in
  List.iter chunk_ids ~f:(fun chunk_id ->
    Action_list.add t.moves_by_chunk.(chunk_id) t.move_count action);
  t.move_count <- t.move_count + 1
;;

let apply_action' t = function
  | Action.Player_move (client_id, move) ->
    let prev_chunk_ids = Board.chunks_that_piece_is_inside t.board client_id in
    let prev_piece = Board.get_piece_exn t.board client_id in
    let new_piece = Board.get_moved_piece t.board prev_piece move in
    let act = Action.Set_player_piece (client_id, new_piece) in
    let _ = Board.apply_action t.board act in
    let all_chunks =
      prev_chunk_ids @ Board.chunks_that_piece_is_inside t.board client_id
    in
    add_action t all_chunks act;
    if Board.should_lock t.board prev_piece move
    then
      [ Action.Set_locked_piece new_piece; Remove_piece client_id ]
      |> List.iter ~f:(fun action ->
        let _ = Board.apply_action t.board action in
        add_action t all_chunks action)
  | Spawn_piece (client_id, _col, _piece_type) as act ->
    let prev_chunk_ids = Board.chunks_that_piece_is_inside t.board client_id in
    let success = Board.apply_action t.board act in
    if success
    then (
      let cur_chunk_ids = Board.chunks_that_piece_is_inside t.board client_id in
      let piece = Board.get_piece_exn t.board client_id in
      add_action
        t
        (prev_chunk_ids @ cur_chunk_ids)
        (Action.Set_player_piece (client_id, piece)))
  | Remove_piece client_id as act ->
    add_action t (Board.chunks_that_piece_is_inside t.board client_id) act;
    let _ = Board.apply_action t.board act in
    ()
  | Set_player_piece (client_id, _piece) as act ->
    let prev_chunk_ids = Board.chunks_that_piece_is_inside t.board client_id in
    let _ = Board.apply_action t.board act in
    let cur_chunk_ids = Board.chunks_that_piece_is_inside t.board client_id in
    add_action t (prev_chunk_ids @ cur_chunk_ids) act
  | Set_locked_piece piece as act ->
    let relevant_chunks =
      Piece.coordinates piece
      |> List.map ~f:Coordinate.col
      |> List.map ~f:(col_to_chunk_id t)
    in
    add_action t relevant_chunks act
  | Delete_row _row as act ->
    let _ = Board.apply_action t.board act in
    add_action t (List.range 0 (Board.num_chunks t.board)) act
  | Delete_chunk chunk_id as act ->
    let _ = Board.apply_action t.board act in
    add_action t (List.range chunk_id (Board.num_chunks t.board)) act
  | Add_chunk as act ->
    (* TODO: change this to be up to date with the render distance *)
    let render_distance = 5 in
    let num_chunks = Board.num_chunks t.board in
    add_action t (List.range (Int.max 0 (num_chunks - render_distance)) num_chunks) act;
    let _ = Board.apply_action t.board act in
    ()
  | Fill_tile coord as act ->
    let chunk_id = col_to_chunk_id t (Coordinate.col coord) in
    add_action t [ chunk_id ] act;
    let _ = Board.apply_action t.board act in
    ()
;;

let apply_action t action =
  apply_action' t action;
  Board.get_full_rows t.board
  |> List.iter ~f:(fun row -> apply_action' t (Action.Delete_row row))
;;
