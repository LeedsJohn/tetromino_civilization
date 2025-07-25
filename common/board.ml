open! Core

module Row_filled_cells_counter = struct
  include Circular_array.Make (struct
      type t = int [@@deriving equal, sexp_of]

      let empty_value = 0
    end)

  let incr t i = set t i (get t i + 1)
  let decr t i = set t i (get t i - 1)
end

type t =
  { data : Chunk_array.t
  ; player_chunk_ids : int Hashtbl.M(Client_id).t
  ; row_filled_cells : Row_filled_cells_counter.t
  }

let copy t =
  { data = Chunk_array.copy t.data
  ; player_chunk_ids = Hashtbl.copy t.player_chunk_ids
  ; row_filled_cells = Row_filled_cells_counter.copy t.row_filled_cells
  }
;;

let add_chunk t =
  if Chunk_array.length t.data < Chunk_array.capacity t.data
  then (
    Chunk_array.add_chunk t.data;
    true)
  else false
;;

let create ~start_num_chunks ~max_num_chunks ~num_rows ~chunk_cols =
  let data =
    Chunk_array.make
      ~start_len:start_num_chunks
      ~rows:num_rows
      ~chunk_cols
      ~max_len:max_num_chunks
  in
  let t =
    { data
    ; player_chunk_ids = Hashtbl.create (module Client_id)
    ; row_filled_cells = Row_filled_cells_counter.make ~len:num_rows
    }
  in
  for _ = 0 to start_num_chunks - 1 do
    let _ = add_chunk t in
    ()
  done;
  t
;;

let num_rows t = Chunk_array.get t.data 0 |> Chunk.num_rows
let num_cols t = (Chunk_array.get t.data 0 |> Chunk.num_cols) * Chunk_array.length t.data
let chunk_cols t = Chunk_array.get t.data 0 |> Chunk.num_cols

let in_bounds t coord =
  let max_col = num_cols t - 1 in
  let row, col = Coordinate.row_col coord in
  Int.between row ~low:0 ~high:(num_rows t - 1) && Int.between col ~low:0 ~high:max_col
;;

let get_chunk_index_and_coord t coord =
  let row, col = Coordinate.row_col coord in
  let chunk_i = col / chunk_cols t in
  chunk_i, Coordinate.make ~row ~col:(col - (chunk_i * chunk_cols t))
;;

let get t coord =
  let chunk_i, coord = get_chunk_index_and_coord t coord in
  let chunk = Chunk_array.get t.data chunk_i in
  Chunk.get chunk coord
;;

let is_filled_or_locked t coord =
  let tile = get t coord in
  Tile.is_filled tile || Tile.is_locked tile
;;

let set t coord tile =
  let chunk_i, chunk_coord = get_chunk_index_and_coord t coord in
  let chunk = Chunk_array.get t.data chunk_i in
  let row = Coordinate.row chunk_coord in
  if is_filled_or_locked t coord then Row_filled_cells_counter.decr t.row_filled_cells row;
  if Tile.is_filled tile || Tile.is_locked tile
  then Row_filled_cells_counter.incr t.row_filled_cells row;
  Chunk.set chunk chunk_coord tile
;;

let player_chunk t client_id =
  let%bind.Option chunk_id = Hashtbl.find t.player_chunk_ids client_id in
  Chunk_array.get t.data chunk_id |> Some
;;

let get_piece t client_id =
  let%bind.Option player_chunk = player_chunk t client_id in
  Chunk.get_piece player_chunk client_id |> Some
;;

let get_piece_exn t client_id = get_piece t client_id |> Option.value_exn

let remove_piece t client_id =
  (match player_chunk t client_id with
   | None -> ()
   | Some chunk -> Chunk.remove_piece chunk client_id);
  Hashtbl.remove t.player_chunk_ids client_id
;;

let set_piece t client_id piece =
  remove_piece t client_id;
  let chunk_id = (Piece.pivot_position piece |> Coordinate.col) / chunk_cols t in
  Hashtbl.set t.player_chunk_ids ~key:client_id ~data:chunk_id;
  let chunk = Chunk_array.get t.data chunk_id in
  Chunk.set_piece chunk client_id piece
;;

let all_player_positions t =
  Hashtbl.keys t.player_chunk_ids
  |> List.map ~f:(fun client_id -> client_id, get_piece_exn t client_id)
;;

let piece_is_inside_filled_or_locked_tile t = Piece.exists ~f:(is_filled_or_locked t)

let update_row_filled_count_after_deleting_chunk t chunk_index =
  for
    col = chunk_index * chunk_cols t to (chunk_index * chunk_cols t) + chunk_cols t - 1
  do
    for row = 0 to num_rows t - 1 do
      set t (Coordinate.make ~row ~col) Tile.empty
    done
  done
;;

let delete_chunk t i =
  if Chunk_array.length t.data > 1 && i < Chunk_array.length t.data
  then (
    let players_to_move =
      Hashtbl.fold t.player_chunk_ids ~init:[] ~f:(fun ~key:client_id ~data:chunk_i acc ->
        if chunk_i > i then (client_id, get_piece_exn t client_id) :: acc else acc)
    in
    List.iter players_to_move ~f:(fun (client_id, _piece) -> remove_piece t client_id);
    update_row_filled_count_after_deleting_chunk t i;
    Chunk_array.delete t.data i;
    List.iter players_to_move ~f:(fun (client_id, piece) ->
      set_piece
        t
        client_id
        (Piece.move piece ~dir:(Coordinate.make ~row:0 ~col:(-chunk_cols t))));
    true)
  else false
;;

let fill_tile t coord =
  set t coord Tile.filled;
  true
;;

let any_below_filled_or_locked_or_under_ground t piece =
  let new_piece = Piece.move piece ~dir:Coordinate.down in
  Piece.exists new_piece ~f:(fun coord -> Coordinate.row coord < 0)
  || piece_is_inside_filled_or_locked_tile t new_piece
;;

let lock_piece t client_id =
  let piece = get_piece_exn t client_id in
  Piece.iter piece ~f:(fun coord ->
    if get t coord |> Tile.is_empty
    then set t coord (Tile.locked (Piece.piece_type piece)));
  remove_piece t client_id
;;

let update_player_positions_after_row_delete t =
  let to_shift_down, to_lock =
    all_player_positions t
    |> List.fold
         ~init:([], [])
         ~f:(fun ((to_shift_down, to_lock) as acc) (client_id, piece) ->
           if Piece.exists piece ~f:(is_filled_or_locked t)
           then
             if any_below_filled_or_locked_or_under_ground t piece
             then to_shift_down, client_id :: to_lock
             else (client_id, piece) :: to_shift_down, to_lock
           else acc)
  in
  List.iter to_lock ~f:(lock_piece t);
  List.iter to_shift_down ~f:(fun (client_id, piece) ->
    set_piece t client_id (Piece.move piece ~dir:Coordinate.down))
;;

let delete_row t row =
  Chunk_array.iter t.data ~f:(fun chunk -> Chunk.delete_row chunk row);
  Row_filled_cells_counter.delete_and_add_row t.row_filled_cells row;
  update_player_positions_after_row_delete t;
  true
;;

let valid_piece_position t piece =
  Piece.for_all piece ~f:(fun coord -> in_bounds t coord && get t coord |> Tile.is_empty)
;;

let spawn_piece t client_id coord piece_type =
  let piece = Piece.make ~piece_type ~pivot_position:coord () in
  if valid_piece_position t piece
  then (
    set_piece t client_id piece;
    true)
  else false
;;

(* OPTIMIZATION: check if the coordinate is higher than the highest filled row in the 
   column and skip the linear scan if it is. *)
let get_drop_count t coord =
  let row = Coordinate.row coord in
  let break = ref false in
  let row_dif = ref 0 in
  while not !break do
    if !row_dif = row
       || is_filled_or_locked
            t
            (Coordinate.add coord (Coordinate.make ~row:(-(!row_dif + 1)) ~col:0))
    then break := true
    else row_dif := !row_dif + 1
  done;
  !row_dif
;;

(* [piece] is the piece before [move] is applied. *)
let should_lock t piece move =
  if piece_is_inside_filled_or_locked_tile t piece
  then true
  else (
    match move with
    | Player_move.Drop -> true
    | Down -> any_below_filled_or_locked_or_under_ground t piece
    | Left | Right | Clockwise | Counter_clockwise -> false)
;;

let get_moved_piece t piece move =
  let new_piece =
    match move with
    | Player_move.Down ->
      if should_lock t piece Player_move.Down
      then piece
      else Piece.move piece ~dir:Coordinate.down
    | Left -> Piece.move piece ~dir:Coordinate.left
    | Right -> Piece.move piece ~dir:Coordinate.right
    | Clockwise -> Piece.rotate piece ~dir:`Clockwise
    | Counter_clockwise -> Piece.rotate piece ~dir:`Counter_clockwise
    | Drop ->
      let drop_count =
        Piece.coordinates piece
        |> List.sort_and_group ~compare:(fun c1 c2 ->
          let col1, col2 = Coordinate.(col c1, col c2) in
          Int.compare col1 col2)
        |> List.map ~f:(fun row_coords ->
          List.min_elt row_coords ~compare:(fun c1 c2 ->
            let row1, row2 = Coordinate.(row c1, row c2) in
            Int.compare row1 row2)
          |> Option.value_exn)
        |> List.map ~f:(get_drop_count t)
        |> List.min_elt ~compare:Int.compare
        |> Option.value_exn
      in
      Piece.move piece ~dir:(Coordinate.make ~row:(-drop_count) ~col:0)
  in
  if Piece.equal new_piece piece || valid_piece_position t new_piece
  then new_piece
  else piece
;;

let player_move t client_id move =
  match get_piece t client_id with
  | None -> false
  | Some old_piece ->
    if piece_is_inside_filled_or_locked_tile t old_piece
    then lock_piece t client_id
    else (
      let new_piece = get_moved_piece t old_piece move in
      set_piece t client_id new_piece;
      if should_lock t old_piece move then lock_piece t client_id);
    true
;;

let apply_action t = function
  | Action.Player_move (client_id, move) -> player_move t client_id move
  | Spawn_piece (client_id, coord, piece_type) -> spawn_piece t client_id coord piece_type
  | Delete_row row -> delete_row t row
  | Delete_chunk chunk_index -> delete_chunk t chunk_index
  | Add_chunk -> add_chunk t
  | Fill_tile coord -> fill_tile t coord
;;

let sorted_coordinate_list t =
  let filled_or_locked =
    List.cartesian_product (List.range 0 (num_rows t)) (List.range 0 (num_cols t))
    |> List.map ~f:(fun (row, col) -> Coordinate.make ~row ~col)
    |> List.map ~f:(fun coord -> coord, get t coord)
    |> List.filter ~f:(fun (_coord, tile) -> not (Tile.is_empty tile))
    |> List.map ~f:(fun (coord, tile) -> coord, Tile.to_char tile)
  in
  let falling =
    all_player_positions t
    |> List.map ~f:snd
    |> List.map ~f:(fun piece ->
      Piece.coordinates piece
      |> List.map ~f:(fun coord ->
        coord, Piece.piece_type piece |> Piece_type.to_char |> Char.lowercase))
    |> List.join
  in
  filled_or_locked @ falling
  |> List.sort ~compare:(fun (coord1, ch1) (coord2, ch2) ->
    if not (Coordinate.equal coord1 coord2)
    then Coordinate.compare coord1 coord2
    else Char.compare ch1 ch2)
;;

let to_string t =
  let tiles = sorted_coordinate_list t in
  let max_col, max_row =
    List.fold tiles ~init:(0, 0) ~f:(fun (max_col, max_row) (coord, _tile) ->
      let row, col = Coordinate.row_col coord in
      Int.max max_col col, Int.max row max_row)
  in
  let row_string row =
    List.range 0 (max_col + 1)
    |> List.map ~f:(fun col ->
      List.find tiles ~f:(fun (coord, _tile) ->
        Coordinate.equal coord (Coordinate.make ~row ~col))
      |> Option.map ~f:snd
      |> Option.value ~default:'.')
    |> String.of_list
  in
  List.range ~stride:(-1) max_row (-1)
  |> List.map ~f:row_string
  |> String.concat ~sep:"\n"
;;

let show t = to_string t |> print_endline

let%expect_test "applying actions" =
  let board = create ~start_num_chunks:1 ~max_num_chunks:1 ~chunk_cols:10 ~num_rows:10 in
  let _ = apply_action board (Action.Fill_tile Coordinate.origin) in
  let id = Client_id.create () in
  let _ =
    apply_action
      board
      (Action.Spawn_piece (id, Coordinate.make ~row:3 ~col:1, Piece_type.T))
  in
  show board;
  [%expect {|
    ttt
    .t.
    ...
    #..
    |}];
  [ Action.Player_move (id, Player_move.Counter_clockwise)
  ; Player_move (id, Player_move.Left)
  ; Player_move (id, Player_move.Drop)
  ]
  |> List.iter ~f:(fun action ->
    let _ = apply_action board action in
    ());
  show board;
  [%expect {|
    T.
    TT
    T.
    #.
    |}];
  let _ = apply_action board (Action.Delete_row 1) in
  show board;
  [%expect {|
    T.
    TT
    #.
    |}]
;;

let%expect_test "adding chunk" =
  let board = create ~start_num_chunks:1 ~max_num_chunks:2 ~num_rows:4 ~chunk_cols:4 in
  let _ = apply_action board (Action.Fill_tile Coordinate.origin) in
  show board;
  [%expect {| # |}];
  let _ = apply_action board Action.Add_chunk in
  let _ = apply_action board (Action.Fill_tile (Coordinate.make ~row:3 ~col:7)) in
  show board;
  [%expect {|
    .......#
    ........
    ........
    #.......
    |}]
;;

let%expect_test "deleting first chunk" =
  let t = create ~start_num_chunks:2 ~max_num_chunks:2 ~num_rows:4 ~chunk_cols:4 in
  let _ = apply_action t (Action.Fill_tile Coordinate.origin) in
  let _ = apply_action t (Action.Fill_tile (Coordinate.make ~row:1 ~col:7)) in
  let client_id = Client_id.create () in
  let spawn_piece_action =
    Action.Spawn_piece (client_id, Coordinate.make ~row:2 ~col:4, Piece_type.O)
  in
  let _ = apply_action t spawn_piece_action in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    ....oo..
    ....oo..
    .......#
    #.......
    (1 1 0 0)
    |}];
  let _ = apply_action t (Action.Delete_chunk 0) in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    oo..
    oo..
    ...#
    ....
    (0 1 0 0)
    |}]
;;

let%expect_test "deleting second chunk" =
  let t = create ~start_num_chunks:2 ~max_num_chunks:2 ~num_rows:4 ~chunk_cols:4 in
  let _ = apply_action t (Action.Fill_tile Coordinate.origin) in
  let _ = apply_action t (Action.Fill_tile (Coordinate.make ~row:1 ~col:7)) in
  let client_id = Client_id.create () in
  let spawn_piece_action =
    Action.Spawn_piece (client_id, Coordinate.make ~row:2 ~col:1, Piece_type.O)
  in
  let _ = apply_action t spawn_piece_action in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    .oo.....
    .oo.....
    .......#
    #.......
    (1 1 0 0)
    |}];
  let _ = apply_action t (Action.Delete_chunk 1) in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    .oo
    .oo
    ...
    #..
    (1 0 0 0)
    |}]
;;
