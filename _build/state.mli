type current

val init_state : Dungeon.t -> current

val player_loc : current -> int * int

type special_key_t

val move : current -> Glut.special_key_t -> current
