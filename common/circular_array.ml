open! Core
include Circular_array_intf

module Make : MAKER =
functor
  (Elem : S)
  ->
  struct
    (* [highest_non_empty = -1 && lowest_non_empty = Int.max_value_30_bits] if the array
       is empty (for portability).
     Probably premature optimization at the cost of readability, but the idea of doing an
     extra unbox every time [set] is called feels kind of sad. *)
    (* TODO: just make this an option... or the nullable type? *)
    type t =
      { data : Elem.t array
      ; mutable start : int
      ; mutable highest_non_empty : int
      ; mutable lowest_non_empty : int
      }
    [@@deriving bin_io, equal, sexp_of]

    let length t = Array.length t.data

    let make ~len =
      { data = Array.create ~len Elem.empty_value
      ; start = 0
      ; highest_non_empty = -1
      ; lowest_non_empty = Int.max_value_30_bits
      }
    ;;

    let reset t =
      t.start <- 0;
      t.highest_non_empty <- -1;
      t.lowest_non_empty <- Int.max_value_30_bits;
      Array.fill t.data ~pos:0 ~len:(Array.length t.data) Elem.empty_value
    ;;

    let highest_non_empty t =
      if t.highest_non_empty = -1 then None else Some t.highest_non_empty
    ;;

    let to_array_index t i =
      if i >= Array.length t.data
      then raise_s [%message "Out of bounds" (i : int) ~length:(length t : int)];
      (i + t.start) % Array.length t.data
    ;;

    let copy { data; start; highest_non_empty; lowest_non_empty } =
      { data = Array.copy data; start; highest_non_empty; lowest_non_empty }
    ;;

    let get t i = t.data.(to_array_index t i)

    let set t i e =
      if i > t.highest_non_empty && not (Elem.equal Elem.empty_value e)
      then t.highest_non_empty <- i;
      if i < t.lowest_non_empty && not (Elem.equal Elem.empty_value e)
      then t.lowest_non_empty <- i;
      t.data.(to_array_index t i) <- e
    ;;

    let to_array t =
      let ar = Array.create ~len:(Array.length t.data) Elem.empty_value in
      if t.highest_non_empty <> -1
      then
        for i = t.lowest_non_empty to t.highest_non_empty do
          ar.(i) <- get t i
        done;
      ar
    ;;

    let to_list t = to_array t |> List.of_array

    let shuffle_up t del_row =
      let i = ref del_row in
      while !i >= Int.max 1 t.lowest_non_empty do
        set t !i (get t (!i - 1));
        i := !i - 1
      done;
      set t 0 Elem.empty_value;
      t.start <- (t.start + 1) % Array.length t.data
    ;;

    let shuffle_down t del_row =
      for i = del_row to Int.min (Array.length t.data - 2) t.highest_non_empty do
        set t i (get t (i + 1))
      done;
      set t (Array.length t.data - 1) Elem.empty_value
    ;;

    (* only lowers [highest_non_empty]. *)
    let reset_highest_non_empty_after_delete t =
      let rec loop i =
        if i < 0
        then t.highest_non_empty <- -1
        else (
          match Elem.equal (get t i) Elem.empty_value with
          | true -> loop (i - 1)
          | false -> t.highest_non_empty <- i)
      in
      loop t.highest_non_empty
    ;;

    let reset_lowest_non_empty_after_delete t =
      let rec loop i =
        if i >= Array.length t.data
        then t.lowest_non_empty <- Int.max_value_30_bits
        else (
          match Elem.equal (get t i) Elem.empty_value with
          | true -> loop (i + 1)
          | false -> t.lowest_non_empty <- i)
      in
      loop (Int.max 0 (t.lowest_non_empty - 1))
    ;;

    let reset_extreme_values t =
      reset_highest_non_empty_after_delete t;
      if t.highest_non_empty = -1
      then t.lowest_non_empty <- Int.max_value_30_bits
      else reset_lowest_non_empty_after_delete t
    ;;

    let delete_and_add_row t i =
      if t.highest_non_empty = -1 || i > t.highest_non_empty
      then ()
      else if i - t.lowest_non_empty <= t.highest_non_empty - i
      then shuffle_up t i
      else shuffle_down t i;
      reset_extreme_values t
    ;;
  end

module Char_circular_array = Make (struct
    type t = char [@@deriving bin_io, equal, sexp_of]

    let empty_value = '.'
  end)

let%expect_test "resetting highest / lowest filled" =
  let ar = Char_circular_array.make ~len:5 in
  List.iter [ 1; 4 ] ~f:(fun i -> Char_circular_array.set ar i '#');
  print_s [%sexp (ar : Char_circular_array.t)];
  [%expect
    {| ((data (. # . . #)) (start 0) (highest_non_empty 4) (lowest_non_empty 1)) |}];
  Char_circular_array.delete_and_add_row ar 4;
  print_s [%sexp (ar : Char_circular_array.t)];
  [%expect
    {| ((data (. # . . .)) (start 0) (highest_non_empty 1) (lowest_non_empty 1)) |}];
  Char_circular_array.delete_and_add_row ar 1;
  print_s [%sexp (ar : Char_circular_array.t)];
  [%expect
    {|
    ((data (. . . . .)) (start 1) (highest_non_empty -1)
     (lowest_non_empty 1073741823))
    |}]
;;

let%expect_test "deleting entry" =
  let t = Char_circular_array.make ~len:5 in
  let full_indices = [ 1; 2; 4 ] in
  List.iter full_indices ~f:(fun i -> Char_circular_array.set t i '#');
  List.range 0 5
  |> List.iter ~f:(fun row ->
    let new_t = Char_circular_array.copy t in
    Char_circular_array.delete_and_add_row new_t row;
    print_s
      [%message
        ""
          ~deleted:(row : int)
          ~data:(new_t : Char_circular_array.t)
          ~array:(Char_circular_array.to_array new_t : char array)]);
  [%expect
    {|
    ((deleted 0)
     (data
      ((data (. # # . #)) (start 1) (highest_non_empty 3) (lowest_non_empty 0)))
     (array (# # . # .)))
    ((deleted 1)
     (data
      ((data (. . # . #)) (start 1) (highest_non_empty 3) (lowest_non_empty 1)))
     (array (. # . # .)))
    ((deleted 2)
     (data
      ((data (. . # . #)) (start 1) (highest_non_empty 3) (lowest_non_empty 1)))
     (array (. # . # .)))
    ((deleted 3)
     (data
      ((data (. # # # .)) (start 0) (highest_non_empty 3) (lowest_non_empty 1)))
     (array (. # # # .)))
    ((deleted 4)
     (data
      ((data (. # # . .)) (start 0) (highest_non_empty 2) (lowest_non_empty 1)))
     (array (. # # . .)))
    |}]
;;

let%expect_test "Deleting bottom row when start is not zero" =
  let t = Char_circular_array.make ~len:10 in
  Char_circular_array.set t 2 '#';
  Char_circular_array.set t 3 '#';
  Char_circular_array.delete_and_add_row t 0;
  print_s [%sexp (t : Char_circular_array.t)];
  Char_circular_array.delete_and_add_row t 0;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {|
    ((data (. . # # . . . . . .)) (start 1) (highest_non_empty 2)
     (lowest_non_empty 1))
    ((data (. . # # . . . . . .)) (start 2) (highest_non_empty 1)
     (lowest_non_empty 0))
  |}]
;;

let%expect_test "Delete top row (nonzero start row)" =
  let t = Char_circular_array.make ~len:5 in
  Char_circular_array.set t 2 '#';
  Char_circular_array.set t 3 '#';
  Char_circular_array.delete_and_add_row t 0;
  Char_circular_array.delete_and_add_row t 4;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {| ((data (. . # # .)) (start 1) (highest_non_empty 2) (lowest_non_empty 1)) |}];
  Char_circular_array.set t 4 '#';
  Char_circular_array.delete_and_add_row t 4;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {| ((data (. . # # .)) (start 1) (highest_non_empty 2) (lowest_non_empty 1)) |}]
;;
