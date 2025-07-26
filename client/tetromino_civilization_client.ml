open! Core
open! Async
open Tetromino_civilization_common
module Protocol = Tetromino_civilization_protocol

let process_move ~conn ~(client : Client.t) ~action =
  match Board.apply_action client.predicted_board action with
  | false -> Deferred.unit
  | true -> Rpc.Rpc.dispatch_exn Protocol.Send_client_move.t conn action
;;

let get_state ~conn =
  let%bind client_id, board = Rpc.Rpc.dispatch_exn Protocol.Get_state.t conn () in
  return { Client.predicted_board = board; confirmed_board = board; client_id }
;;

let bruh conn =
  print_endline "bruh";
  let%bind client = get_state ~conn in
  let%bind () =
    process_move
      ~conn
      ~client
      ~action:
        (Action.Spawn_piece (client.client_id, Coordinate.make ~row:5 ~col:5, Piece_type.Z))
  in
  Board.show client.predicted_board;
  Deferred.unit
;;
