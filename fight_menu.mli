(* [render_menu fight] renders [fight] *)
val render_menu : State.fight -> unit

(* [render_menu fight] renders an attack based on [fight] *)
val render_attack : State.fight -> unit

(* [render_player_damage] renders the player taking damagee *)
val render_player_damage : unit -> unit
