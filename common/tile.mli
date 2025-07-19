open! Core

type t [@@deriving equal, sexp_of]

val to_char : t -> char
val is_empty : t -> bool
val empty : t
val i : t
val o : t
val t : t
val s : t
val z : t
val j : t
val l : t
