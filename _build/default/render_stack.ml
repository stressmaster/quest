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

let stack = Stack.create ()

let string_of_scene a =
  match a with
  | DungeonRender -> "DungeonRender"
  | FightRender -> "FightRender"
  | SpiralRender -> "SpiralRender"
  | AttackRender -> "AttackRender"
  | ScreenshakeRender -> "ScreenshakeRender"
  | WinRender -> "WinRender"
  | GameoverRender -> "GameoverRender"
  | StartRender -> "StartRender"
  | LoadingRender -> "LoadingRender"

let debug_stack r p =
  match (r, p) with
  | r, true -> ">>>> PUSH " ^ string_of_scene r
  | r, false -> "<<< POP " ^ string_of_scene r

let stack_peek () = Stack.top stack

let stack_push x =
  print_string (debug_stack x true);
  print_newline ();
  Stack.push x stack

let stack_pop () =
  print_string (debug_stack (stack_peek ()) false);
  print_newline ();
  ignore (Stack.pop stack);
  print_string (string_of_scene (stack_peek ()) ^ " is now top");
  print_newline ()

let _ = stack_push DungeonRender
