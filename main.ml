(* [main] renders the game*)
let main () =
  Engine.init_engine Magic_numbers.texture_list Magic_numbers.w
    Magic_numbers.h Magic_numbers.x_length Magic_numbers.y_length

let _ = main ()
