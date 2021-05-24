(** This module renders the game. It includes functions for making
    squares and screen effects. *)

(** abstract type representing a square *)
type square

(** [new_square x y width height texture] instantiates a square with
    same-named attributes [x] [y] [width] [height] and [texutre] *)
val new_square : float -> float -> float -> float -> string -> square

(** [render_square square] rendeers [square] *)
val render_square : square:square -> unit

(** [render_square_flases square1 square2 flashes] alternately renders
    [square1] and [square2] for [flashes] flashes *)
val render_square_flashes : square -> square -> int -> unit

(** [render_screen_shake i] shakes the screen proportional to [i] times *)
val render_screen_shake : int -> unit
