open! Core
open Tetromino_civilization_common

(* i'd like to generalize this but my only reason is for fun so i'm not going to (yet) *)
module Column = struct
  (* [heighest_filled] = -1 if the column is empty *)
  type t =
    { data : Tile.t array
    ; mutable start : int
    ; mutable heighest_filled : int
    }
  [@@deriving sexp_of]

  let length t = Array.length t.data
  let make ~len = { data = Array.create ~len Tile.Empty; start = 0; heighest_filled = -1 }
  let to_array_index t i = (i + t.start) % Array.length t.data

  let copy { data; start; heighest_filled } =
    { data = Array.copy data; start; heighest_filled }
  ;;

  let get t i = t.data.(to_array_index t i)

  let set t i tile =
    (match i > t.heighest_filled, tile with
     | true, Tile.Full -> t.heighest_filled <- i
     | false, Empty | false, Full | true, Empty -> ());
    t.data.(to_array_index t i) <- tile
  ;;

  let shuffle_up t del_row =
    let i = ref del_row in
    while !i > 0 do
      set t !i (get t (!i - 1));
      i := !i - 1
    done;
    set t t.start Empty;
    t.start <- (t.start + 1) % Array.length t.data
  ;;

  let shuffle_down t del_row =
    for i = del_row to t.heighest_filled - 1 do
      set t i (get t (i + 1))
    done;
    set t t.heighest_filled Empty
  ;;

  (* only lowers [heighest_filled]. *)
  let reset_heighest_filled_after_delete t =
    let rec loop i =
      if i < 0
      then t.heighest_filled <- -1
      else (
        match get t i with
        | Empty -> loop (i - 1)
        | Full -> t.heighest_filled <- i)
    in
    loop t.heighest_filled
  ;;

  let delete t i =
    if t.heighest_filled = -1 || i > t.heighest_filled
    then ()
    else if i <= t.heighest_filled - i
    then shuffle_up t i
    else shuffle_down t i;
    reset_heighest_filled_after_delete t
  ;;
end

type t = Column.t array

let make ~rows ~cols = Array.init cols ~f:(fun _ -> Column.make ~len:rows)
let num_rows t = Column.length t.(0)
let num_cols t = Array.length t

let get t coordinate =
  let row, col = Coordinate.row_col coordinate in
  Column.get t.(col) row
;;

let set t coordinate tile =
  let row, col = Coordinate.row_col coordinate in
  Column.set t.(col) row tile
;;

let delete_row t row = Array.iter t ~f:(fun col -> Column.delete col row)

let show t =
  let rows = num_rows t in
  let cols = num_cols t in
  let max_filled_row =
    Array.map t ~f:(fun col -> col.heighest_filled)
    |> Array.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let num_empty_rows = rows - max_filled_row - 1 in
  let start_row =
    if num_empty_rows <= 2
    then rows - 1
    else (
      print_endline [%string "truncated %{num_empty_rows#Int} empty rows"];
      max_filled_row)
  in
  let r = ref start_row in
  while !r >= 0 do
    print_string [%string "%{!r#Int:3} "];
    for c = 0 to cols - 1 do
      get t (Coordinate.make ~row:!r ~col:c)
      |> Tile.to_char
      |> Char.to_string
      |> print_string
    done;
    print_endline "";
    r := !r - 1
  done;
  print_string "    ";
  for c = 0 to cols do
    print_string (c % 10 |> Int.to_string)
  done
;;

let%expect_test "resetting heighest filled column" =
  let col = Column.make ~len:5 in
  List.iter [ 1; 4 ] ~f:(fun i -> Column.set col i Tile.Full);
  print_s [%sexp (col : Column.t)];
  [%expect {| ((data (Empty Full Empty Empty Full)) (start 0) (heighest_filled 4)) |}];
  Column.delete col 4;
  print_s [%sexp (col : Column.t)];
  [%expect {| ((data (Empty Full Empty Empty Empty)) (start 0) (heighest_filled 1)) |}];
  Column.delete col 1;
  print_s [%sexp (col : Column.t)];
  [%expect {| ((data (Empty Empty Empty Empty Empty)) (start 0) (heighest_filled -1)) |}]
;;

let%expect_test "deleting column entry" =
  let t = Column.make ~len:5 in
  let full_indices = [ 1; 2; 4 ] in
  List.iter full_indices ~f:(fun i -> Column.set t i Tile.Full);
  List.range 0 5
  |> List.iter ~f:(fun row ->
    let new_t = Column.copy t in
    Column.delete new_t row;
    print_s [%message "" ~deleted:(row : int) ~column:(new_t : Column.t)]);
  [%expect
    {|
    ((deleted 0)
     (column ((data (Empty Full Full Empty Full)) (start 1) (heighest_filled 3))))
    ((deleted 1)
     (column
      ((data (Empty Empty Full Empty Full)) (start 1) (heighest_filled 3))))
    ((deleted 2)
     (column
      ((data (Empty Empty Full Empty Full)) (start 1) (heighest_filled 3))))
    ((deleted 3)
     (column ((data (Empty Full Full Full Empty)) (start 0) (heighest_filled 3))))
    ((deleted 4)
     (column
      ((data (Empty Full Full Empty Empty)) (start 0) (heighest_filled 2))))
    |}]
;;

let%expect_test "deleting row" =
  let t = make ~rows:10 ~cols:5 in
  for row = 0 to 6 do
    let c1 = Coordinate.make ~row ~col:(row % 5) in
    let c2 = Coordinate.make ~row ~col:((row + 1) % 5) in
    set t c1 Tile.Full;
    set t c2 Tile.Full
  done;
  show t;
  [%expect
    {|
    truncated 3 empty rows
      6 .##..
      5 ##...
      4 #...#
      3 ...##
      2 ..##.
      1 .##..
      0 ##...
        012345
    |}];
  delete_row t 1;
  delete_row t 4;
  show t;
  [%expect
    {|
    truncated 5 empty rows
      4 .##..
      3 #...#
      2 ...##
      1 ..##.
      0 ##...
        012345
    |}]
;;
