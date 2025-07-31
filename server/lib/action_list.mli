open! Core
open Tetromino_civilization_common

type t

val make : int -> t
val add : t -> int -> Action.t -> unit
val get_actions_after : t -> int -> (int * Action.t) list
