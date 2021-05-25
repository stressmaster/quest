(** Representation of an item.

    This module represents the data associated with an item, including
    data about the type and stats of an item. Includes methods for
    creating items and getting item attributes. *)

(** type representing item stats*)
type item_stats = {
  sprite : string;
  name : string;
  depth : int;
  modifier : int;
}

(** abstract type representing an item *)
type t =
  | Weapon of item_stats
  | Armor of item_stats
  | NoItem

(** [empty_item] is the empty item*)
val empty_item : t

(** [create_item_hard itype sprite name depth modifier] is an [item] of
    [itype] with sprite [sprite] name [name] depth [depth] and modifier
    [modifier] *)
val create_item_hard : string -> string -> string -> int -> int -> t

(** [get_item_type item] is the type of [item] *)
val get_item_type : t -> string

(** [get_item_sprite item] is the sprite of [item] *)
val get_item_sprite : t -> string

(** [get_item_depth item] is the depth of [item] *)
val get_item_depth : t -> int

(** [get_item_name item] is the name of [item] *)
val get_item_name : t -> string

(** [get_item_modifier item] is the modifier of [item] *)
val get_item_modifier : t -> int

(** [get_item_modifier depth itype magic_numbers] is an item of depth
    [depth] type [itype] and other attributes based on [magic_numbers] *)
val create_item : int -> bool -> Magic_numbers.t -> t
