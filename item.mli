type item_stats = {
  sprite : string;
  name : string;
  depth : int;
  modifier : int;
}

type t =
  | Weapon of item_stats
  | Armor of item_stats
  | NoItem

val empty_item : t

val create_item_hard : string -> string -> string -> int -> int -> t

val get_item_type : t -> string

val get_item_sprite : t -> string

val get_item_depth : t -> int

val get_item_name : t -> string

val get_item_modifier : t -> int

val create_item : int -> bool -> Magic_numbers.t -> t
