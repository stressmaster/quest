(** Rendering of a fight between the player and a monster.

    This module renders a fight between the player and a monster based
    on the game state *)

(** [render_menu fight] renders [fight] *)
val render_menu : State.fight -> unit

(** [render_attack fight] renders an attack based on [fight] *)
val render_attack : State.fight -> unit

(** [render_player_damage] renders the player taking damagee *)
val render_player_damage : unit -> unit
