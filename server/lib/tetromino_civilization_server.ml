open! Core
open! Async
open Tetromino_civilization_common
module Protocol = Tetromino_civilization_protocol

let port = 31415

let get_state board =
  Rpc.Rpc.implement Protocol.Init.t (fun _state () -> return (Client_id.create (), board))
;;

let move_reconciliation action_list =
  Rpc.Pipe_rpc.implement Protocol.Move_reconciliation.t (fun _conn _query ->
    let last_sent_move = ref (List.length !action_list) in
    let r, w = Pipe.create () in
    Clock.every (Time_float.Span.of_sec 1.0) (fun () ->
      let al = !action_list in
      let num_new_moves = List.length al - !last_sent_move in
      Pipe.write_without_pushback_if_open w (List.take al num_new_moves);
      last_sent_move := List.length al);
    return (Ok r))
;;

let process_client_move board action_list =
  Rpc.One_way.implement Protocol.Client_move.t (fun _state action ->
    let success = Board.apply_action board action in
    if success then action_list := action :: !action_list)
;;

let implementations board action_list =
  Rpc.Implementations.create_exn
    ~implementations:
      [ get_state board
      ; process_client_move board action_list
      ; move_reconciliation action_list
      ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;

let do_thing () =
  let board =
    Board.create ~start_num_chunks:1 ~max_num_chunks:10 ~num_rows:50 ~chunk_cols:6
  in
  let action_list = ref [] in
  let implementations = implementations board action_list in
  let hostname = Unix.gethostname () in
  printf "Serving %s:%d/\n%!" hostname port;
  let%bind server =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~implementations
      ~initial_connection_state:(fun _initiated_from _addr _inet _con ->
        print_endline "i'm connected!")
      ()
  in
  Cohttp_async.Server.close_finished server
;;
