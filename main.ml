(* [main] renders the game*)
let main () =
  Bitmap.maximum_live := 15000000;
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r;
  Engine.init_engine Magic_numbers.texture_list Magic_numbers.w
    Magic_numbers.h Magic_numbers.x_length Magic_numbers.y_length

let _ = main ()
