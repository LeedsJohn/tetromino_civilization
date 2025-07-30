open! Core
open! Async

(* val process_move : conn:Rpc.Connection.t -> client:Client.t -> action:Action.t -> unit
*)
val bruh : Rpc.Connection.t -> unit Deferred.t
