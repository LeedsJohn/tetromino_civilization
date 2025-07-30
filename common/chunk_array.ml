open! Core

module Ar = Circular_array.Make (struct
    type t = Chunk.t [@@deriving bin_io, sexp_of]
  end)

type t = Ar.t [@@deriving bin_io, sexp_of]

let make ~rows ~chunk_cols ~max_len ~start_len =
  if start_len < 1 || max_len < start_len
  then
    raise_s
      [%message
        "it must be that 1 <= start_len <= max_len" (start_len : int) (max_len : int)];
  let t = Ar.make ~max_len ~start_len:0 in
  for _ = 0 to start_len - 1 do
    Ar.append_some t (Chunk.make ~rows ~cols:chunk_cols)
  done;
  t
;;

let length = Ar.length
let capacity = Ar.capacity
let get = Ar.get_some_exn
let set = Ar.set_some

let add_chunk t =
  let dummy_chunk = get t 0 in
  let rows, cols = Chunk.num_rows dummy_chunk, Chunk.num_cols dummy_chunk in
  Ar.append_some t (Chunk.make ~rows ~cols)
;;

let delete t i =
  if length t = 1
  then raise_s [%message "Cannot delete last chunk" ~length:(length t : int)];
  Ar.delete t i
;;

let iter t ~f = List.range 0 (length t) |> List.map ~f:(get t) |> List.iter ~f
let copy = Ar.copy
