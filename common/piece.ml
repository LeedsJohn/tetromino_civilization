open! Core

type t =
  { piece_type : Piece_type.t
  ; pivot_position : Coordinate.t
  ; rotate_count : int
  }
[@@deriving equal, sexp_of]

let make ?(rotate_count = 0) ~piece_type ~pivot_position () =
  { piece_type; pivot_position; rotate_count }
;;

let pivot_position t = t.pivot_position
let piece_type t = t.piece_type

(* it is probably a sin to encode this here but OH WELL!
   here's how i'm defining offsets just to make writing this (and hopefully reading) this
   easier
   ul u  ur
   l     r
   dl d  dr
*)
let ul = Coordinate.make ~row:1 ~col:(-1)
let u = Coordinate.up
let ur = Coordinate.make ~row:1 ~col:1
let l = Coordinate.left
let r = Coordinate.right
let dl = Coordinate.make ~row:(-1) ~col:(-1)
let d = Coordinate.down
let dr = Coordinate.make ~row:(-1) ~col:1

let rotation_data rotate_count = function
  | Piece_type.O -> u, ur, r
  | I ->
    [| l, r, Coordinate.make ~row:0 ~col:2; d, u, Coordinate.make ~row:2 ~col:0 |].(rotate_count
                                                                                    % 2)
  | J -> [| l, r, dr; dl, d, u; ul, l, r; d, u, ur |].(rotate_count)
  | L -> [| dl, l, r; ul, u, d; l, r, ur; u, d, dr |].(rotate_count)
  | T -> [| l, r, d; l, u, d; l, r, u; d, u, r |].(rotate_count)
  | S -> [| dl, d, r; u, r, dr |].(rotate_count % 2)
  | Z -> [| l, d, dr; d, r, ur |].(rotate_count % 2)
;;

let coordinates t =
  let a, b, c = rotation_data t.rotate_count t.piece_type in
  [ t.pivot_position
  ; Coordinate.add t.pivot_position a
  ; Coordinate.add t.pivot_position b
  ; Coordinate.add t.pivot_position c
  ]
;;

module C = Container.Make0 (struct
    module Elt = struct
      type t = Coordinate.t [@@deriving equal]
    end

    type nonrec t = t

    let iter = `Custom (fun t ~f -> coordinates t |> List.iter ~f)
    let fold t ~init ~f = coordinates t |> List.fold ~init ~f
    let length = `Custom (fun _t -> 4)
  end)

let fold = C.fold
let iter = C.iter
let count = C.count
let sum = C.sum
let exists = C.exists
let mem = C.mem
let for_all = C.for_all
let find_map = C.find_map
let find = C.find
let to_list = C.to_list
let min_elt = C.min_elt
let max_elt = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until
let to_array = C.to_array
let length = C.length
let is_empty = C.is_empty

let rotate t ~dir =
  let offset =
    match dir with
    | `Clockwise -> 1
    | `Counter_clockwise -> -1
  in
  { t with rotate_count = (t.rotate_count + offset) % 4 }
;;

let move t ~dir = { t with pivot_position = Coordinate.add t.pivot_position dir }

let repr t =
  let min_row, min_col =
    fold
      t
      ~init:(Int.max_value_30_bits, Int.max_value_30_bits)
      ~f:(fun (min_row, min_col) coord ->
        let row, col = Coordinate.row_col coord in
        Int.min min_row row, Int.min min_col col)
  in
  let adj_coordinates =
    { t with
      pivot_position =
        Coordinate.add t.pivot_position (Coordinate.make ~row:(-min_row) ~col:(-min_col))
    }
    |> coordinates
  in
  let n =
    match piece_type t with
    | Piece_type.O -> 2
    | I -> 4
    | J | L | T | S | Z -> 3
  in
  let res = Array.make_matrix ~dimx:n ~dimy:n '.' in
  for row = 0 to n - 1 do
    for col = 0 to n - 1 do
      if List.mem adj_coordinates (Coordinate.make ~row ~col) ~equal:Coordinate.equal
      then res.(n - 1 - row).(col) <- '#'
    done
  done;
  Array.map res ~f:String.of_array |> String.concat_array ~sep:"\n"
;;

let%expect_test "rotating" =
  let show_piece_rotations piece_type =
    let num_rotations = 5 in
    let piece = make ~piece_type ~pivot_position:Coordinate.origin () in
    let lines =
      List.range 0 num_rotations
      |> List.folding_map ~init:piece ~f:(fun piece _i ->
        rotate piece ~dir:`Clockwise, repr piece)
      |> List.map ~f:String.split_lines
      |> List.map ~f:List.to_array
    in
    for row = 0 to (List.hd_exn lines |> Array.length) - 1 do
      List.iter lines ~f:(fun ar -> print_string (ar.(row) ^ " "));
      print_endline ""
    done;
    print_endline ""
  in
  List.iter [ Piece_type.I; O; T; J; L; S; Z ] ~f:show_piece_rotations;
  [%expect
    {|
    .... #... .... #... ....
    .... #... .... #... ....
    .... #... .... #... ....
    #### #... #### #... ####

    ## ## ## ## ##
    ## ## ## ## ##

    ... .#. ... #.. ...
    ### ##. .#. ##. ###
    .#. .#. ### #.. .#.
    
    ... .#. ... ##. ...
    ### .#. #.. #.. ###
    ..# ##. ### #.. ..#

    ... ##. ... #.. ...
    ### .#. ..# #.. ###
    #.. .#. ### ##. #..

    ... #.. ... #.. ...
    .## ##. .## ##. .##
    ##. .#. ##. .#. ##.

    ... .#. ... .#. ...
    ##. ##. ##. ##. ##.
    .## #.. .## #.. .##
    |}]
;;
