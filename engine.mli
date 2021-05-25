(** The engine of this game

    Initializes graphics, gain inputs from user, render new graphics
    based on new state, and repeat. *)

(** [init_engine texture_list w h x_length y_length] initilizes the game
    engine *)
val init_engine : string list -> int -> int -> int -> int -> unit
