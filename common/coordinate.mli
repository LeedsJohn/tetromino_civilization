(** (row, column) pair. Can be used as either a vector or a point. *)
open! Core

type t [@@deriving bin_io, compare, equal, sexp]

val make : row:int -> col:int -> t
val row_col : t -> int * int
val row : t -> int
val col : t -> int
val add : t -> t -> t
val manhattan_distance : t -> t -> int

(** Unit vector pointing down. *)
val down : t

(** Unit vector pointing up. *)
val up : t

(** Unit vector pointing left. *)
val left : t

(** Unit vector pointing right. *)
val right : t

(** The point (0, 0). *)
val origin : t
