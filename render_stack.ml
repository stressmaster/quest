type scene =
  | DungeonRender
  | FightRender
  | SpiralRender
  | AttackRender
  | ScreenshakeRender
  | WinRender
  | GameoverRender
  | StartRender

let stack = Stack.create ()

let stack_peek () = Stack.top stack

let stack_push x = Stack.push x stack

let stack_pop () = ignore (Stack.pop stack)

let _ =
  stack_push DungeonRender;
  stack_push StartRender
