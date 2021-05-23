(* a type representing a monster *)
type t = {
  name : string;
  sprite : string;
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
  max_hp : int;
}

(* [get_monster_string m] is an attack string of [m] *)
val get_monster_string : t -> string

(* [get_monster_HP m] is the HP of [m] *)
val get_monster_HP : t -> int

(* [get_monster_max_HP m] is the HP of [m] *)
val get_monster_max_HP : t -> int

(* [get_monster magic_numbers] is a monster in [magic_numbers] *)
val get_monster : Magic_numbers.t -> t
