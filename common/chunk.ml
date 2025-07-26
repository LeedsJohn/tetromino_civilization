open! Core

module Column = Circular_array.Make (struct
    type t = Tile.t [@@deriving bin_io, equal, sexp_of]

    let empty_value = Tile.empty
  end)

type t =
  { data : Column.t array
  ; players : Piece.t Hashtbl.M(Client_id).t
  }
[@@deriving bin_io, equal, sexp_of]

let make ~rows ~cols =
  let data = Array.init cols ~f:(fun _ -> Column.make ~len:rows) in
  let players = Hashtbl.create (module Client_id) in
  { data; players }
;;

let copy t = { data = Array.map t.data ~f:Column.copy; players = Hashtbl.copy t.players }
let num_rows t = Column.length t.data.(0)
let num_cols t = Array.length t.data

let get t coordinate =
  let row, col = Coordinate.row_col coordinate in
  Column.get t.data.(col) row
;;

let set t coordinate tile =
  let row, col = Coordinate.row_col coordinate in
  Column.set t.data.(col) row tile
;;

let get_piece t client_id = Hashtbl.find_exn t.players client_id
let remove_piece t client_id = Hashtbl.remove t.players client_id
let set_piece t client_id piece = Hashtbl.set t.players ~key:client_id ~data:piece

let reset t =
  Array.iter t.data ~f:Column.reset;
  Hashtbl.clear t.players
;;

let delete_row t row = Array.iter t.data ~f:(fun col -> Column.delete_and_add_row col row)

let show ?(with_labels = true) t =
  let rows = num_rows t in
  let cols = num_cols t in
  let max_filled_row =
    Array.map t.data ~f:Column.highest_non_empty
    |> Array.map ~f:(Option.value ~default:0)
    |> Array.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let falling_coords =
    Hashtbl.data t.players
    |> List.map ~f:(fun piece ->
      Piece.coordinates piece |> List.map ~f:(fun coord -> coord, Piece.piece_type piece))
    |> List.join
  in
  let max_falling_row =
    List.map falling_coords ~f:fst
    |> List.map ~f:Coordinate.row
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let max_filled_row = Int.max max_filled_row max_falling_row in
  let num_empty_rows = rows - max_filled_row - 1 in
  let start_row =
    if num_empty_rows <= 2
    then rows - 1
    else (
      if with_labels
      then print_endline [%string "truncated %{num_empty_rows#Int} empty rows"];
      max_filled_row)
  in
  let row = ref start_row in
  while !row >= 0 do
    if with_labels then print_string [%string "%{!row#Int:3} "];
    for col = 0 to cols - 1 do
      let coord = Coordinate.make ~row:!row ~col in
      let c =
        let tile = get t coord in
        if not (Tile.is_empty tile)
        then Tile.to_char tile
        else (
          let piece_type =
            List.find falling_coords ~f:(fun (falling_coord, _tile) ->
              Coordinate.equal coord falling_coord)
          in
          match piece_type with
          | None -> '.'
          | Some (_coord, piece_type) -> Piece_type.to_char piece_type)
      in
      Char.to_string c |> print_string
    done;
    print_endline "";
    row := !row - 1
  done;
  if with_labels
  then (
    print_string "    ";
    for c = 0 to cols do
      print_string (c % 10 |> Int.to_string)
    done)
;;

let%expect_test "deleting row" =
  let t = make ~rows:10 ~cols:5 in
  for row = 0 to 6 do
    let c1 = Coordinate.make ~row ~col:(row % 5) in
    let c2 = Coordinate.make ~row ~col:((row + 1) % 5) in
    set t c1 Tile.filled;
    set t c2 Tile.filled
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
  |}];
  delete_row t 0;
  show t;
  [%expect
    {|
    truncated 6 empty rows
      3 .##..
      2 #...#
      1 ...##
      0 ..##.
        012345
    |}]
;;
