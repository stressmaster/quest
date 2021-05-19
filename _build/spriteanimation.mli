type animation

type id = string

val init_animations : (id * string list) list -> unit

val get_sprite : id -> string

val step_animation : unit -> unit
