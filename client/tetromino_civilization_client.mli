open! Core
open! Async
open Tetromino_civilization_common

val process_move
  :  conn:Rpc.Connection.t
  -> client:Client.t
  -> action:Action.t
  -> unit Deferred.t

val bruh : Rpc.Connection.t -> unit Deferred.t
