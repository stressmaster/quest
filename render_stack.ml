type animation =
  | DungeonRender
  | FightRender
  | SpiralRender

let stack = Stack.create ()

let string_of_animation a =
  match a with
  | DungeonRender -> "DungeonRender"
  | FightRender -> "FightRender"
  | SpiralRender -> "SpiralRender"

let debug_stack r p =
  match (r, p) with
  | r, true -> ">>>> PUSH " ^ string_of_animation r
  | r, false -> "<<< POP " ^ string_of_animation r

let stack_peek () = Stack.top stack

let stack_push x =
  print_string (debug_stack x true);
  print_newline ();
  Stack.push x stack

let stack_pop () =
  print_string (debug_stack (stack_peek ()) false);
  print_newline ();
  ignore (Stack.pop stack);
  print_string (string_of_animation (stack_peek ()) ^ " is now top");
  print_newline ()

let _ = stack_push DungeonRender
