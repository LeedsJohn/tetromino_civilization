(** Holds all the [Chunk.t]s that make up a [Board.t]. *)
open! Core

type t [@@deriving bin_io]

val make : rows:int -> chunk_cols:int -> max_len:int -> start_len:int -> t

(** How many chunks are currently in the [t]. *)
val length : t -> int

val capacity : t -> int
val get : t -> int -> Chunk.t
val set : t -> int -> Chunk.t -> unit

(** Adds an empty [Chunk.t] to the end of the [t]. *)
val add_chunk : t -> unit

val delete : t -> int -> unit
val iter : t -> f:(Chunk.t -> unit) -> unit
val copy : t -> t
