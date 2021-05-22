type t = {
  name : string;
  sprite : string;
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
  max_hp : int;
}

val monster_move : t -> string

val get_monster_string : t -> string

val get_monster_HP : t -> int

val get_monster_max_HP : t -> int

val get_monster : Magic_numbers.t -> t
