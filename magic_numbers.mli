type item_tiers = {
  tier_one_weapon : string;
  tier_one_armor : string;
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
  weapon_pickup : string;
  armor_pickup : string;
  empty_item : string;
}

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
  item_tiers : item_tiers;
}

val init : int -> t

val texture_list : string list

val update : t -> unit

val get_magic : t ref

val length : int

val w : int

val square_height : height:float -> float

val h : int

val x_length : int

val y_length : int

val width : float

val height : float
