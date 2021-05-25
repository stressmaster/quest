(** Represents the fonts of the game. Includes functions for making and
    rendering fonts. *)

(** abstract type representing a font *)
type bitmap_string

(** [new_font str x y w h] makes a font of [str] at position ([x],[y])
    with width [w] and height [h] *)
val new_font :
  string -> float -> float -> float -> float -> bitmap_string

(** [render_font ?spacing font] renders [font] with [spacing] *)
val render_font : ?spacing:float -> bitmap_string -> unit
