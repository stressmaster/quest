(** Representation of dynamic state data

    This module contains the state of the game playthrough, including
    the current dungeon, the list of visited dungeons, and the current
    fight*)

(** The abstract type of values representing the state*)
type current

(** The abstract type of values representing the action taken during
    fights*)
type action =
  | Run
  | Recover
  | Attack

(** The abstract type of values representing the action taken after
    dying.*)
type game_over_action =
  | Quit
  | Revive
  | Restart

(** The abstract type of values representing the action taken in the
    start menu. NewGame will start a new playthrough, while Continue
    will continue an existing playthrough.*)
type start_menu_action =
  | NewGame
  | Continue

(** The data value representing all the necessary information needed to
    run a fight. This includes the current monster, the player's health,
    etc. *)
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

(** [reset_fight current] resets the fight data to the defaults when out
    of fight*)
val reset_fight : current -> unit

(** [init_state str] creates a new state (including the initial dungeon,
    player location, etc) based on the JSON represented by str*)
val init_state : string -> current

(** [init_state_from_save str] creates a new state (including the list
    of visited dungeons, player location, etc.) based on the JSON
    represented by str*)
val init_state_from_save : string -> current

(** [player_loc current] returns coordinates of player location *)
val player_loc : current -> int * int

(** [controller current] updates current based on special key input (eg.
    up key)*)
val controller : current -> Glut.special_key_t -> current

(** [typing_move current key] updates current based on the regular key
    input (eg. 'a')*)
val typing_move : current -> int -> current

(** [fighting_case current key] creates a string during the fight based
    on key and the data contained in current*)
val fighting_case : current -> int -> string

(** [curr_room current] returns the Dungeon value that represents the
    room*)
val curr_room : current -> Dungeon.t

(** [in_fight current] returns whether or not the player is in a fight*)
val in_fight : current -> bool

(** [curr_fight current] returns the fight value that represents the
    current battle*)
val curr_fight : current -> fight

(** [curr_game_over current] returns the action taken in the game-over
    menu*)
val curr_game_over : current -> game_over_action

(** [curr_start_menu current] returns the action taken in the start menu*)
val curr_start_menu : current -> start_menu_action

(** [render_inventory current] renders the inventory in the bottom right
    corner of the screen*)
val render_inventory : current -> unit

(** [render_exp current] renders the current level and experience points
    in the top right corner of the screen*)
val render_exp : current -> unit

(** [check_time_limit current] updates current, ending the player's
    attack if they have taken too long*)
val check_time_limit : current -> current

(** [curr_lives current] returns the current number of lives*)
val curr_lives : current -> int
