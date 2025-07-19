open! Core

(* TODO: change this to a [or_null] *)
type t = Piece_type.t option [@@deriving equal, sexp_of]

let to_char = function
  | None -> '.'
  | Some Piece_type.I -> 'I'
  | Some O -> 'O'
  | Some T -> 'T'
  | Some S -> 'S'
  | Some Z -> 'Z'
  | Some J -> 'J'
  | Some L -> 'L'
;;

let is_empty = Option.is_none
let empty = None
let i = Some Piece_type.I
let o = Some Piece_type.O
let t = Some Piece_type.T
let s = Some Piece_type.S
let z = Some Piece_type.Z
let j = Some Piece_type.J
let l = Some Piece_type.L
