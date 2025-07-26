open! Core
open! Async
open Tetromino_civilization_common
module Protocol = Tetromino_civilization_protocol

let port = 31415

let dummy =
  Rpc.Rpc.implement Protocol.Dummy.t (fun _state s -> return (String.uppercase s))
;;

let get_state board =
  Rpc.Rpc.implement Protocol.Get_state.t (fun _state () ->
    return (Client_id.create (), board))
;;

let process_client_move board =
  Rpc.Rpc.implement Protocol.Send_client_move.t (fun _state action ->
    let _ = Board.apply_action board action in
    Deferred.unit)
;;

let implementations board =
  Rpc.Implementations.create_exn
    ~implementations:[ dummy; get_state board; process_client_move board ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;

let do_thing () =
  let board =
    Board.create ~start_num_chunks:1 ~max_num_chunks:10 ~num_rows:8 ~chunk_cols:8
  in
  let implementations = implementations board in
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
