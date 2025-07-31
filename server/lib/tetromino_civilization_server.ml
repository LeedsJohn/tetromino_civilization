open! Core
open! Async
open Tetromino_civilization_common
module Protocol = Tetromino_civilization_protocol

let port = 31415

let get_state state =
  Rpc.Rpc.implement Protocol.Init.t (fun _state () ->
    let client_id = Client_id.create () in
    State.add_client state client_id;
    return (client_id, State.get_board state))
;;

let state_update state =
  Rpc.Pipe_rpc.implement Protocol.State_update.t (fun _conn client_id ->
    let r, w = Pipe.create () in
    Clock.every (Time_float.Span.of_sec 1.0) (fun () ->
      Pipe.write_without_pushback_if_open w (State.get_new_moves state client_id));
    return (Ok r))
;;

let process_client_move state =
  Rpc.One_way.implement Protocol.Client_move.t (fun _state action ->
    State.apply_action state action)
;;

let implementations state =
  Rpc.Implementations.create_exn
    ~implementations:[ get_state state; process_client_move state; state_update state ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;

let do_thing () =
  let state =
    State.create ~start_num_chunks:1 ~max_num_chunks:10 ~num_rows:50 ~chunk_cols:6
  in
  let implementations = implementations state in
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
