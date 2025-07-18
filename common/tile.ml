open! Core

type t =
  | Empty
  | Full
[@@deriving equal, sexp_of]
