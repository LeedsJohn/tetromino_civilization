open! Core
open Tetromino_civilization_common

type t

val create : unit -> t

(** A player is not allowed to move if they exceed the rate limit. *)
val is_allowed_to_move : t -> bool

val previous_move_time : t -> Time_ns.t

(** A player must spawn one of each piece type before repeating a piece type. (bag
    shuffling) *)
val is_allowed_to_spawn : t -> Piece_type.t -> bool

(** Called after a client spawns a new piece. Must be called to ensure fair bag shuffling. *)
val spawned_piece : t -> Piece_type.t -> unit

(** Called after a client makes a move. Must be called for rate limiting purposes. *)
val made_move : t -> unit
