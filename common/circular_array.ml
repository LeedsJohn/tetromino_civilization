open! Core
include Circular_array_intf

module Make : MAKER =
functor
  (Elem : S)
  ->
  struct
    type t =
      { data : Elem.t Option_array.t
      ; mutable start : int
      ; mutable end_ : int
      ; mutable lowest_non_empty : int option
      ; mutable highest_non_empty : int option
      ; mutable length : int
      }
    [@@deriving bin_io, sexp_of]

    let make ~max_len ~start_len =
      if start_len < 0 || max_len < 0 || start_len > max_len
      then
        raise_s
          [%message
            "it must be that 0 <= start_len <= max_len" (start_len : int) (max_len : int)];
      { data = Option_array.create ~len:max_len
      ; start = 0
      ; end_ = (if start_len = max_len then 0 else start_len)
      ; length = start_len
      ; lowest_non_empty = None
      ; highest_non_empty = None
      }
    ;;

    let length t = t.length
    let capacity t = Option_array.length t.data
    let lowest_non_empty t = t.lowest_non_empty
    let highest_non_empty t = t.highest_non_empty

    let copy { data; start; end_; length; lowest_non_empty; highest_non_empty } =
      { data = Option_array.copy data
      ; start
      ; end_
      ; length
      ; lowest_non_empty
      ; highest_non_empty
      }
    ;;

    let lowest_non_empty_or_default t =
      Option.value t.lowest_non_empty ~default:Int.max_value_30_bits
    ;;

    let highest_non_empty_or_default t = Option.value t.highest_non_empty ~default:(-1)
    let to_array_index t i = (i + t.start) % Option_array.length t.data
    let index_in_bounds t = Int.between ~low:0 ~high:(length t - 1)

    let assert_index_in_bounds t i =
      if not (index_in_bounds t i)
      then raise_s [%message "Index out of bounds" (i : int) ~length:(length t : int)]
    ;;

    let get t i =
      assert_index_in_bounds t i;
      Option_array.get t.data (to_array_index t i)
    ;;

    let get_some_exn t i = get t i |> Option.value_exn

    let set_some t i e =
      assert_index_in_bounds t i;
      if i > highest_non_empty_or_default t then t.highest_non_empty <- Some i;
      if i < lowest_non_empty_or_default t then t.lowest_non_empty <- Some i;
      Option_array.set_some t.data (to_array_index t i) e
    ;;

    (** raises if the array has no [some] entries. *)
    let update_lowest_non_empty_exn ?(start = 0) t =
      let rec aux i =
        if get t i |> Option.is_some then t.lowest_non_empty <- Some i else aux (i + 1)
      in
      aux start
    ;;

    (** raises if the array has no [some] entries. *)
    let update_highest_non_empty_exn ?start t =
      let rec aux i =
        if get t i |> Option.is_some then t.highest_non_empty <- Some i else aux (i - 1)
      in
      let start = Option.value start ~default:(length t - 1) in
      aux start
    ;;

    let set_none t i =
      assert_index_in_bounds t i;
      if i = lowest_non_empty_or_default t && i = highest_non_empty_or_default t
      then (
        t.lowest_non_empty <- None;
        t.highest_non_empty <- None)
      else if i = lowest_non_empty_or_default t
      then update_lowest_non_empty_exn ~start:(i + 1) t
      else if i = highest_non_empty_or_default t
      then update_highest_non_empty_exn ~start:(i - 1) t;
      Option_array.set_none t.data (to_array_index t i)
    ;;

    let set t i e =
      match e with
      | None -> set_none t i
      | Some e -> set_some t i e
    ;;

    let to_array t = Array.init (length t) ~f:(get t)
    let to_some_array t = to_array t |> Array.filter_opt
    let to_list t = to_array t |> List.of_array
    let to_some_list t = to_some_array t |> List.of_array

    (* TODO: change this to utilize highest and lowest non empty *)
    let shuffle_up t del_row =
      let i = ref del_row in
      while !i > 0 do
        set t !i (get t (!i - 1));
        i := !i - 1
      done;
      set_none t 0;
      t.start <- (t.start + 1) % Option_array.length t.data;
      t.lowest_non_empty <- Option.map t.lowest_non_empty ~f:(fun n -> n - 1);
      t.highest_non_empty <- Option.map t.highest_non_empty ~f:(fun n -> n - 1)
    ;;

    let shuffle_down t del_row =
      for i = del_row to length t - 2 do
        set t i (get t (i + 1))
      done;
      set_none t (length t - 1);
      t.end_ <- (t.end_ - 1) % Option_array.length t.data
    ;;

    let delete t i =
      assert_index_in_bounds t i;
      if i <= length t - i then shuffle_up t i else shuffle_down t i;
      t.length <- t.length - 1
    ;;

    let append t e =
      if length t = Option_array.length t.data then raise_s [%message "Array full"];
      t.length <- t.length + 1;
      set t (length t - 1) e;
      t.end_ <- (t.end_ + 1) % Option_array.length t.data
    ;;

    let append_none t = append t None
    let append_some t e = append t (Some e)

    let set_length t len =
      if not (Int.between len ~low:0 ~high:(Option_array.length t.data))
      then
        raise_s
          [%message
            "len must be between 0 and [max_length]"
              ~requested_length:(len : int)
              ~max_length:(Option_array.length t.data : int)];
      while length t < len do
        append_none t
      done;
      while length t > len do
        delete t (length t - 1)
      done
    ;;
  end

module Char_circular_array = Make (struct
    type t = char [@@deriving bin_io, equal, sexp_of]
  end)

let%expect_test "getting and setting elements" =
  let t = Char_circular_array.make ~max_len:4 ~start_len:0 in
  String.iter "abcd" ~f:(Char_circular_array.append_some t);
  List.range 0 4
  |> List.map ~f:(Char_circular_array.get_some_exn t)
  |> String.of_list
  |> print_endline;
  [%expect {| abcd |}]
;;

let%expect_test "deleting" =
  let to_string t = Char_circular_array.to_some_list t |> String.of_list in
  let t = Char_circular_array.make ~max_len:20 ~start_len:0 in
  String.iter "0123456789" ~f:(Char_circular_array.append_some t);
  List.iter [ 0; 8; 0; 3; 3 ] ~f:(fun i ->
    Char_circular_array.delete t i;
    print_endline [%string "Deleting index %{i#Int}: %{to_string t}"]);
  [%expect
    {|
    Deleting index 0: 123456789
    Deleting index 8: 12345678
    Deleting index 0: 2345678
    Deleting index 3: 234678
    Deleting index 3: 23478
    |}]
;;

let%expect_test "resetting highest / lowest filled" =
  let t = Char_circular_array.make ~max_len:5 ~start_len:5 in
  let print_highest_and_lowest t =
    print_s
      [%message
        ""
          ~lowest_non_empty:(Char_circular_array.lowest_non_empty t : int option)
          ~highest_non_empty:(Char_circular_array.highest_non_empty t : int option)]
  in
  List.iter [ 1; 4 ] ~f:(fun i -> Char_circular_array.set_some t i '#');
  print_highest_and_lowest t;
  [%expect {| ((lowest_non_empty (1)) (highest_non_empty (4))) |}];
  Char_circular_array.delete t 4;
  Char_circular_array.append_none t;
  print_highest_and_lowest t;
  [%expect {| ((lowest_non_empty (1)) (highest_non_empty (1))) |}];
  Char_circular_array.delete t 1;
  Char_circular_array.append_none t;
  print_highest_and_lowest t;
  [%expect {| ((lowest_non_empty ()) (highest_non_empty ())) |}]
;;

let%expect_test "deleting entry" =
  let t = Char_circular_array.make ~max_len:5 ~start_len:5 in
  let to_string t =
    Char_circular_array.to_list t
    |> List.map ~f:(Option.value ~default:'.')
    |> String.of_list
  in
  let full_indices = [ 1; 2; 4 ] in
  List.iter full_indices ~f:(fun i -> Char_circular_array.set_some t i '#');
  print_s [%message "" ~original:(to_string t : string)];
  List.range 0 5
  |> List.iter ~f:(fun row ->
    let new_t = Char_circular_array.copy t in
    Char_circular_array.delete new_t row;
    print_s [%message "" ~deleted:(row : int) ~array:(to_string new_t : string)]);
  [%expect
    {|
    (original .##.#)
    ((deleted 0) (array ##.#))
    ((deleted 1) (array .#.#))
    ((deleted 2) (array .#.#))
    ((deleted 3) (array .###))
    ((deleted 4) (array .##.))
    |}]
;;

let%expect_test "Deleting bottom row when start is not zero" =
  let t = Char_circular_array.make ~max_len:10 ~start_len:10 in
  Char_circular_array.set_some t 2 '#';
  Char_circular_array.set_some t 3 '#';
  Char_circular_array.delete t 0;
  print_s [%sexp (t : Char_circular_array.t)];
  Char_circular_array.delete t 0;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {|
    ((data (() () (#) (#) () () () () () ())) (start 1) (end_ 0)
     (lowest_non_empty (1)) (highest_non_empty (2)) (length 9))
    ((data (() () (#) (#) () () () () () ())) (start 2) (end_ 0)
     (lowest_non_empty (0)) (highest_non_empty (1)) (length 8))
    |}]
;;

let%expect_test "Delete top row (nonzero start row)" =
  let t = Char_circular_array.make ~max_len:5 ~start_len:5 in
  Char_circular_array.set_some t 2 '#';
  Char_circular_array.set_some t 3 '#';
  Char_circular_array.delete t 0;
  Char_circular_array.append_none t;
  Char_circular_array.delete t 4;
  Char_circular_array.append_none t;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {|
    ((data (() () (#) (#) ())) (start 1) (end_ 1) (lowest_non_empty (1))
     (highest_non_empty (2)) (length 5))
    |}];
  Char_circular_array.set_some t 4 '#';
  Char_circular_array.delete t 4;
  print_s [%sexp (t : Char_circular_array.t)];
  [%expect
    {|
    ((data (() () (#) (#) ())) (start 1) (end_ 0) (lowest_non_empty (1))
     (highest_non_empty (2)) (length 4))
    |}]
;;

let%expect_test "Deleting row between highest and lowest non empty" =
  let t = Char_circular_array.make ~max_len:8 ~start_len:8 in
  Char_circular_array.set_some t 0 '#';
  Char_circular_array.set_some t 7 '#';
  let new_t = Char_circular_array.copy t in
  Char_circular_array.delete new_t 1;
  print_s [%sexp (new_t : Char_circular_array.t)];
  [%expect
    {|
    ((data (() (#) () () () () () (#))) (start 1) (end_ 0) (lowest_non_empty (0))
     (highest_non_empty (6)) (length 7))
    |}];
  let new_t = Char_circular_array.copy t in
  Char_circular_array.delete new_t 6;
  print_s [%sexp (new_t : Char_circular_array.t)];
  [%expect
    {|
    ((data ((#) () () () () () (#) ())) (start 0) (end_ 7) (lowest_non_empty (0))
     (highest_non_empty (6)) (length 7))
    |}]
;;
