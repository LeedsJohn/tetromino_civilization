open! Core

type t [@@deriving bin_io, sexp_of]

val make : len:int -> t
val copy : t -> t
val length : t -> int
val get : t -> int -> Tile.t
val set : t -> int -> Tile.t -> unit
val reset : t -> unit
val delete_and_add_row : t -> int -> unit
val highest_non_empty : t -> int option
