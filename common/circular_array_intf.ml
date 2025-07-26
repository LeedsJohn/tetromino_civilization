(** Constant length array. Designed to be efficient when deleting from either the
    beginning or end. *)
open! Core

module type S = sig
  type t [@@deriving bin_io, equal, sexp_of]

  val empty_value : t
end

module type Circular_array = sig
  type elem
  type t [@@deriving bin_io, equal, sexp_of]

  val make : len:int -> t
  val reset : t -> unit
  val length : t -> int
  val copy : t -> t
  val get : t -> int -> elem
  val set : t -> int -> elem -> unit

  (** The added row is at the end of the array. *)
  val delete_and_add_row : t -> int -> unit

  (** Index of highest non empty item (or [None] if the array is empty). *)
  val highest_non_empty : t -> int option

  val to_array : t -> elem array
  val to_list : t -> elem list
end

module type MAKER = functor (Elem : S) -> Circular_array with type elem := Elem.t

module type Intf = sig
  module type S = S
  module type Circular_array = Circular_array
  module type MAKER = MAKER

  module Make : MAKER
end
