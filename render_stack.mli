(** This module contains the stack of scenes to render.*)

(** the type representing a scene *)
type scene =
  | DungeonRender
  | FightRender
  | SpiralRender
  | AttackRender
  | ScreenshakeRender
  | WinRender
  | GameoverRender
  | StartRender

(** [stack_push scene] pushes [scene] onto the top of the stack *)
val stack_push : scene -> unit

(** [stack_pop ()] removes the top most scene from the stack *)
val stack_pop : unit -> unit

(** [stack_peek ()] returns the top most scene from the stack *)
val stack_peek : unit -> scene
