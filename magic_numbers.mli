(** Representation of magic numbers.

    This module represents constants used throughout the game for
    rendering, as well as coherent sets of constants for dungeons.
    Includes methods for obtaining such constants. *)

(** a type representing item attributes that are lists *)
type plurals = {
  tier_one_materials : string list;
  tier_two_materials : string list;
  tier_three_materials : string list;
  tier_one_weapons : string list;
  tier_two_weapons : string list;
  tier_three_weapons : string list;
  tier_one_armors : string list;
  tier_two_armors : string list;
  tier_three_armors : string list;
  tier_one_prefixes : string list;
  tier_two_prefixes : string list;
  tier_three_prefixes : string list;
}

(** a type representing item-related attributes *)
type items = {
  tier_one_weapon : string;
  tier_one_armor : string;
  tier_two_weapon : string;
  tier_two_armor : string;
  tier_three_weapon : string;
  tier_three_armor : string;
  plurals : plurals;
  weapon_pickup : string;
  armor_pickup : string;
  empty_item : string;
}

(** a type representing various game attributes *)
type t = {
  wall : string;
  path : string;
  entrance : string;
  exit : string;
  player : string;
  timer : string;
  darkness : string;
  animations : (string * string list) list;
  health : int;
  npcs : (string * string) list;
  monsters : (int * string * string * int * string list) list;
  items : items;
}

(** [init x] is a magic_numbers determined by [x]. Requires: [x] is
    between 0 and [length] *)
val init : int -> t

(** [texture_list] is a list of textures *)
val texture_list : string list

(** [update new_magic_numbers] sets the current magic_numbers instance
    to [new_magic_numbers] *)
val update : t -> unit

(** [get_magic] is a ref to the current magic_numbers instance *)
val get_magic : t ref

(** [length] is the number of magic_numbers instances available *)
val length : int

(** [w] is the default width of the window *)
val w : int

(** [h] is the default height of the window *)
val h : int

(** [square_height height] is the pixel height of a square given that a
    column of squares contains [height] squares *)
val square_height : height:float -> float

(** [x_length] is the default number of squares in a row of the window*)
val x_length : int

(** [y_length] is the default number of squares in a column of the
    window*)
val y_length : int

(** [width] is the default pixel width of a square *)
val width : float

(** [height] is the default pixel height of a square *)
val height : float
