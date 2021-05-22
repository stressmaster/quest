type current

type action =
  | Run
  | Recover
  | Attack

type game_over_action =
  | Quit
  | Revive
  | Restart

type fight = {
  mutable spiraled : bool;
  mutable action : action;
  mutable attacking : bool;
  mutable monster : Monsters.t;
  mutable monster_string : string;
  mutable monster_health : int;
  mutable player_health : int;
  mutable typing_limit : int;
  mutable input_string : string;
}

val reset_fight : current -> unit

(* [init_state] assigns player to starting location in dungeon *)
val init_state : string -> current

(* [player_loc] returns cords of current player location *)
val player_loc : current -> int * int

(* [controller] updates current based on key input *)
val controller : current -> Glut.special_key_t -> current

val typing_move : current -> int -> current

val fighting_case : current -> int -> string

val curr_room : current -> Dungeon.t

val in_fight : current -> bool

val curr_fight : current -> fight

val curr_game_over : current -> game_over_action

val render_inventory : current -> unit

val check_time_limit : current -> current
