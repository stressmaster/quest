type t = {
  wall : string;
  path : string;
  entrance : string;
  exit : string;
  player : string;
  timer : string;
  darkness : string;
  empty_item : string;
  weapon_pickup : string;
  armor_pickup : string;
  tier_one_weapon : string;
  tier_one_armor : string;
  animations : (string * string list) list;
  w : int;
  square_height : height:float -> float;
  h : int;
  x_length : int;
  y_length : int;
  health : int;
  width : float;
  height : float;
  npcs : (string * string) list;
  tier_one_prefixes : string list;
  tier_two_prefixes : string list;
  tier_three_prefixes : string list;
  tier_one_materials : string list;
  tier_two_materials : string list;
  tier_three_materials : string list;
  tier_one_weapons : string list;
  tier_two_weapons : string list;
  tier_three_weapons : string list;
  tier_one_armors : string list;
  tier_two_armors : string list;
  tier_three_armors : string list;
  monsters : (int * string * string * int * string list) list;
  texture_list : string list;
}

val init : int -> t

val update : t -> unit

val get_magic : t ref

val length : int
