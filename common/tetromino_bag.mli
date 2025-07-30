(** Tetromino bag. Randomly shuffles bag of tetrominos and distributes one of each type
    before repeating. *)
open! Core

type t [@@deriving bin_io, sexp_of]

val create : unit -> t
val mem : t -> Piece_type.t -> bool
val remove : t -> Piece_type.t -> unit

(** Returns a random [Piece_type.t] still in the bag. Resets the bag if it is empty after
    removing the piece type. *)
val get : t -> Piece_type.t
