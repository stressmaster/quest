type current

val init_state : Dungeon.t -> current

val player_loc : current -> int * int

val move : current -> Glut.special_key_t -> current
