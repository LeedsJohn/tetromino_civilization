open! Core
open! Async
open Tetromino_civilization_common
module Protocol = Tetromino_civilization_protocol

let process_move ~conn ~(client : Client.t) ~action =
  match Board.apply_action client.predicted_board action with
  | false -> Out_channel.flush Out_channel.stdout
  | true ->
    Out_channel.flush Out_channel.stdout;
    Rpc.One_way.dispatch_exn Protocol.Client_move.t conn action
;;

let state_update ~conn ~(client : Client.t) =
  let%bind pipe, _ =
    Rpc.Pipe_rpc.dispatch_exn Protocol.State_update.t conn client.client_id
  in
  Pipe.iter pipe ~f:(fun action_list ->
    List.iter action_list ~f:(fun action ->
      let _ = Board.apply_action client.confirmed_board action in
      ());
    client.predicted_board <- Board.copy client.confirmed_board;
    Deferred.unit)
;;

let get_state ~conn =
  let%bind client_id, board = Rpc.Rpc.dispatch_exn Protocol.Init.t conn () in
  return { Client.predicted_board = board; confirmed_board = board; client_id }
;;

let random_piece_action client_id =
  let piece_type = List.random_element_exn Piece_type.all in
  let col = Random.int_incl 0 5 in
  Action.Spawn_piece (client_id, col, piece_type)
;;

let john conn (client : Client.t) =
  let down_count = ref 0 in
  let get_thingy e low high = List.create ~len:(Random.int_incl low high) e in
  let weighted_moves =
    [ Player_move.Down, 3, 6
    ; Left, 2, 4
    ; Right, 2, 4
    ; Counter_clockwise, 1, 3
    ; Clockwise, 1, 3
    ]
    |> List.map ~f:(fun (e, low, high) -> get_thingy e low high)
    |> List.join
  in
  Clock.every (Time_float.Span.of_sec 0.05) (fun () ->
    if Random.int 10 = 0
    then (
      let action =
        if !down_count = -1
        then (
          down_count := 0;
          random_piece_action client.client_id)
        else if !down_count >= 10
        then (
          down_count := -1;
          Action.Player_move (client.client_id, Player_move.Drop))
        else (
          let move = List.random_element_exn weighted_moves in
          (match move with
           | Player_move.Down -> down_count := !down_count + 1
           | _ -> ());
          Action.Player_move (client.client_id, move))
      in
      process_move ~conn ~client ~action;
      Board.show client.predicted_board;
      print_endline ""));
  Deferred.unit
;;

(* let _ = In_channel.input_line In_channel.stdin in
  process_move
    ~conn
    ~client
    ~action:(Action.Player_move (client.client_id, Player_move.Down));
  print_endline "showing board";
  Out_channel.flush Out_channel.stdout;
  Board.show client.predicted_board;
  john conn client *)

let run_stuff conn (client : Client.t) =
  let action = random_piece_action client.client_id in
  process_move ~conn ~client ~action;
  john conn client
;;

let bruh conn =
  print_endline "bruh";
  let%bind client = get_state ~conn in
  let pipe = state_update ~conn ~client in
  let%bind () = pipe
  and () = run_stuff conn client in
  Deferred.unit
;;
