open! Core
open! Async
open! Tetromino_civilization_common

module Client_move = struct
  let t =
    Rpc.One_way.create
      ~name:"client moves"
      ~version:0
      ~bin_msg:[%bin_type_class: Action.t]
  ;;
end

module Init = struct
  let t =
    Rpc.Rpc.create
      ~name:"init"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Client_id.t * Board.t]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Move_reconciliation = struct
  let t =
    Rpc.Pipe_rpc.create
      ~name:"move reconciliation"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Action.t list]
      ~bin_error:[%bin_type_class: unit]
      ()
  ;;
end
