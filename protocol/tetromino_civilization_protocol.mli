open! Core
open! Async_rpc_kernel
open! Tetromino_civilization_common

module Send_client_move : sig
  val t : (Action.t, unit) Rpc.Rpc.t
end

module Get_state : sig
  val t : (unit, Client_id.t * Board.t) Rpc.Rpc.t
end

module Send_confirmed_actions : sig
  val t : (Action.t list, unit) Rpc.Rpc.t
end

module Dummy : sig
  val t : (string, string) Rpc.Rpc.t
end
