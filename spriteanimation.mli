type t

val init_animation : string -> string list -> t

val get_sprite : t -> string

val animation_ticker : t -> t
