open! Core

module Tile_circular_array = Circular_array.Make (struct
    type t = Tile.t [@@deriving bin_io, sexp_of]
  end)

module Ar = Tile_circular_array

type t = Ar.t [@@deriving bin_io, sexp_of]

let make ~len = Ar.make ~max_len:len ~start_len:len
let copy = Ar.copy
let length = Ar.length

let get t i =
  match Ar.get t i with
  | None -> Tile.empty
  | Some tile -> tile
;;

let set t i tile = if Tile.is_empty tile then Ar.set_none t i else Ar.set_some t i tile
let reset t = Ar.set_length t 0

let delete_and_add_row t row =
  Ar.delete t row;
  Ar.append_none t
;;

let highest_non_empty = Ar.highest_non_empty
