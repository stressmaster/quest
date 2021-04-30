type direction =
  | Up
  | Down
  | Left
  | Right

type walker = {
  mutable current_pos : int * int;
  mutable dir : direction;
  mutable x_min : int;
  mutable x_max : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable step_history : (int * int) list;
  mutable steps_since_turn : int;
}

val init_walker : int -> int -> int -> int -> walker

val walk : int -> walker -> (int * int) list
