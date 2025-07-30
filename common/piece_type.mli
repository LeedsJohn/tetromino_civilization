(** Types of tetrominos. *)
open! Core

type t =
  | I
  | O
  | T
  | S
  | Z
  | J
  | L
[@@deriving bin_io, compare, equal, sexp]

val to_char : t -> char
val all : t list
