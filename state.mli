(* abstract type representing current player location *)
type current

(* [init_state] assigns player to starting location in dungeon *)
val init_state : string -> current

(* [player_loc] returns cords of current player location *)
val player_loc : current -> int * int

(* [move] updates player location *)
val map_move_controller : current -> Glut.special_key_t -> current

val curr_room : current -> Dungeon.t
