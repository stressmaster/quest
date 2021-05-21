type t

val get_npc : int -> Magic_numbers.t -> t

val get_npc_speech : t -> string

val get_npc_sprite : t -> string

val npc_list_length : Magic_numbers.t -> int
