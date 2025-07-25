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
[@@deriving compare, equal, sexp]

val to_char : t -> char
