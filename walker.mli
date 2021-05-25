(** This module contains the algorithm for creating a random dungeon.*)

(** a type representing a direction *)
type direction =
  | Up
  | Down
  | Left
  | Right

(** the abstract type representing a walker *)
type walker = {
  mutable furthest_pos : int * int;
  mutable start_pos : int * int;
  mutable current_pos : int * int;
  mutable dir : direction;
  mutable x_min : int;
  mutable x_max : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable step_history : (int * int) list;
  mutable steps_since_turn : int;
}

(** [init_walker xmin xmax ymin ymax start] is a walker that walks
    within the rectangluar bounds as specified by [xmin], [xmax],
    [ymin], [ymax] and starts walking from the coordinate [start] *)
val init_walker : int -> int -> int -> int -> int * int -> walker

(** [walk steps w] is a list of coordinates that walker [w] visited on
    its walk of length [steps] *)
val walk : int -> walker -> (int * int) list
