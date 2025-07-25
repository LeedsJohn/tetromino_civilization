open! Core

type t [@@deriving compare, equal, sexp]

val to_char : t -> char
val is_empty : t -> bool
val is_locked : t -> bool
val is_filled : t -> bool
val empty : t
val locked : Piece_type.t -> t
val filled : t
