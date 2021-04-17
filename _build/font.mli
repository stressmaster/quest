type bitmap_string

val new_font :
  string -> float -> float -> float -> float -> bitmap_string

val render_font : bitmap_string -> unit
