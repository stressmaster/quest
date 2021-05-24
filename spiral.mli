(** Representation of spiral animation algorithm.

    This module renders the spiral which appears upon entering a fight.
    It implements this based on the system timer. *)

(** [render_spiral fight x y] renders the spiral from the center of
    dimensions x and y, and upon completion sets fight.spiraled to false*)
val render_spiral : State.fight -> int -> int -> unit
