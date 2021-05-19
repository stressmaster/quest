type bitmap_string

val new_font :
  string -> float -> float -> float -> float -> bitmap_string

val render_font : ?spacing:float -> bitmap_string -> unit
