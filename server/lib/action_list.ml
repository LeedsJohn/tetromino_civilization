open! Core
open Tetromino_civilization_common

module Action_list_array = Circular_array.Make (struct
    type t = (int * Action.t) option [@@deriving bin_io, equal, sexp_of]

    let empty_value = None
  end)

type t = Action_list_array.t

let make len = Action_list_array.make ~len

let add t move_count action =
  Action_list_array.delete_and_add_row t 0;
  Action_list_array.set t (Action_list_array.length t - 1) (Some (move_count, action))
;;

let get_actions_starting_with t n =
  Action_list_array.to_list t
  |> List.filter_opt
  |> List.filter ~f:(fun (move_count, _action) -> move_count >= n)
;;
