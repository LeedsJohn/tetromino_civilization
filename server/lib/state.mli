open! Core
open Tetromino_civilization_common

type t

val create
  :  start_num_chunks:int
  -> max_num_chunks:int
  -> num_rows:int
  -> chunk_cols:int
  -> t

val get_new_moves : t -> Client_id.t -> Action.t list
val add_client : t -> Client_id.t -> unit
val apply_action : t -> Action.t -> unit
val get_board : t -> Board.t
