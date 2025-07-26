open! Core
open! Async
open! Tetromino_civilization_common

module Send_client_move = struct
  let t =
    Rpc.Rpc.create
      ~name:"client moves"
      ~version:0
      ~bin_query:[%bin_type_class: Action.t]
      ~bin_response:[%bin_type_class: unit]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Get_state = struct
  let t =
    Rpc.Rpc.create
      ~name:"get state"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Client_id.t * Board.t]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Send_confirmed_actions = struct
  let t =
    Rpc.Rpc.create
      ~name:"confirmed moves"
      ~version:0
      ~bin_query:[%bin_type_class: Action.t list]
      ~bin_response:[%bin_type_class: unit]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Dummy = struct
  let t =
    Rpc.Rpc.create
      ~name:"dummy"
      ~version:0
      ~bin_query:[%bin_type_class: string]
      ~bin_response:[%bin_type_class: string]
      ~include_in_error_count:Only_on_exn
  ;;
end
