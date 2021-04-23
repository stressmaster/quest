type square

val new_square : float -> float -> float -> float -> string -> square

val render_square : square:square -> unit

val render_square_flashes : square -> square -> int -> unit
