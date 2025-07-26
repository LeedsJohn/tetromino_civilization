open! Core

(* I wanted to functorize this and combine this and [Column] but there's too many
   differences when one has immutable content and one has mutable content. *)

type t =
  { data : Chunk.t array
  ; mutable start : int
  ; mutable end_ : int
  }
[@@deriving bin_io, sexp_of]

let make ~rows ~chunk_cols ~max_len ~start_len =
  let end_ =
    if start_len < max_len
    then start_len
    else if start_len = max_len
    then 0
    else raise_s [%message "start len > max len" (start_len : int) (max_len : int)]
  in
  { data = Array.init max_len ~f:(fun _ -> Chunk.make ~rows ~cols:chunk_cols)
  ; start = 0
  ; end_
  }
;;

let copy t = { data = Array.map t.data ~f:Chunk.copy; start = t.start; end_ = t.end_ }

let length t =
  if t.end_ = t.start
  then Array.length t.data
  else if t.end_ > t.start
  then t.end_ - t.start
  else Array.length t.data - t.start + t.end_
;;

let capacity t = Array.length t.data
let to_array_index t i = (i + t.start) % Array.length t.data

let get t i =
  if i >= length t then raise_s [%message "Out of bounds" (i : int)];
  t.data.(to_array_index t i)
;;

let set t i chunk =
  if i >= length t then raise_s [%message "Out of bounds" (i : int)];
  t.data.(to_array_index t i) <- chunk
;;

let num_rows t = Chunk.num_rows t.data.(0)
let chunk_cols t = Chunk.num_cols t.data.(0)

let add_chunk t =
  t.data.(t.end_) <- Chunk.make ~rows:(num_rows t) ~cols:(chunk_cols t);
  t.end_ <- (t.end_ + 1) % Array.length t.data
;;

let shuffle_up t del_i =
  let i = ref del_i in
  while !i >= 1 do
    set t !i (get t (!i - 1));
    i := !i - 1
  done;
  t.start <- (t.start + 1) % Array.length t.data
;;

let shuffle_down t del_i =
  for i = del_i to t.end_ - 2 do
    set t i (get t (i + 1))
  done;
  t.end_ <- (t.end_ - 1) % Array.length t.data
;;

let delete t i = if i <= length t - i then shuffle_up t i else shuffle_down t i

let iter t ~f =
  for i = 0 to length t - 1 do
    f (get t i)
  done
;;

let%expect_test "adding chunk" =
  let t = make ~rows:2 ~chunk_cols:1 ~max_len:2 ~start_len:1 in
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((data
      (((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))
       ((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))))
     (start 0) (end_ 1))
    |}];
  add_chunk t;
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((data
      (((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))
       ((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))))
     (start 0) (end_ 0))
    |}]
;;

let%expect_test "deleting chunk" =
  let t = make ~rows:2 ~chunk_cols:1 ~max_len:2 ~start_len:2 in
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((data
      (((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))
       ((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))))
     (start 0) (end_ 0))
    |}];
  delete t 0;
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((data
      (((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))
       ((data
         (((data (Empty Empty)) (start 0) (highest_non_empty -1)
           (lowest_non_empty 1073741823))))
        (players ()))))
     (start 1) (end_ 0))
    |}];
  print_s [%sexp (length t : int)];
  [%expect {| 1 |}]
;;
