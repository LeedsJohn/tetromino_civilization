(** Constant length array. Designed to be efficient when deleting from either the
    beginning or end. *)
open! Core

module type S = sig
  type t [@@deriving bin_io, sexp_of]
end

module type Circular_array = sig
  type elem
  type t [@@deriving bin_io, sexp_of]

  val make : max_len:int -> start_len:int -> t
  val length : t -> int
  val copy : t -> t
  val get : t -> int -> elem option
  val get_some_exn : t -> int -> elem
  val set : t -> int -> elem option -> unit
  val set_some : t -> int -> elem -> unit
  val set_none : t -> int -> unit
  val delete : t -> int -> unit

  (** Returns the index of the lowest non-empty entry or [None] if the array is empty. *)
  val lowest_non_empty : t -> int option

  (** Returns the index of the highest non-empty entry or [None] if the array is empty. *)
  val highest_non_empty : t -> int option

  (** Adds element to the back *)
  val append : t -> elem option -> unit

  val append_none : t -> unit
  val append_some : t -> elem -> unit
  val to_array : t -> elem option array
  val to_some_array : t -> elem array
  val to_list : t -> elem option list
  val to_some_list : t -> elem list
end

module type MAKER = functor (Elem : S) -> Circular_array with type elem := Elem.t

module type Intf = sig
  module type S = S
  module type Circular_array = Circular_array
  module type MAKER = MAKER

  module Make : MAKER
end
