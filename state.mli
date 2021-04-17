type current

type action =
  | Run
  | Recover
  | Attack

type fight = {
  mutable spiraled : bool;
  mutable action : action;
  mutable attacking : bool;
  mutable monster : Dungeon.monster;
  mutable monster_string : string;
  mutable input_string : string;
}

(* [init_state] assigns player to starting location in dungeon *)
val init_state : string -> current

(* [player_loc] returns cords of current player location *)
val player_loc : current -> int * int

(* [controller] updates current based on key input *)
val controller : current -> Glut.special_key_t -> current

val typing_move : current -> int -> current

val curr_room : current -> Dungeon.t

val in_fight : current -> bool

val curr_fight : current -> fight
