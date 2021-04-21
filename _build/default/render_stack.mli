type animation =
  | DungeonRender
  | FightRender
  | SpiralRender

val stack_push : animation -> unit

val stack_pop : unit -> unit

val stack_peek : unit -> animation
