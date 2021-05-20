type scene =
  | DungeonRender
  | FightRender
  | SpiralRender
  | AttackRender
  | ScreenshakeRender
  | WinRender
  | GameoverRender
  | StartRender
  | LoadingRender

val stack_push : scene -> unit

val stack_pop : unit -> unit

val stack_peek : unit -> scene
