open! Core
open! Async_rpc_kernel
open! Tetromino_civilization_common

module Client_move : sig
  val t : Action.t Rpc.One_way.t
end

module Init : sig
  val t : (unit, Client_id.t * Board.t) Rpc.Rpc.t
end

module State_update : sig
  val t : (Client_id.t, Action.t list, unit) Rpc.Pipe_rpc.t
end
