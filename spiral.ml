type direction =
  | Up
  | Down
  | Right
  | Left

let instantiate_dungeon_cells x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      Hashtbl.add dungeon_cells (counter_x, counter_y) false
    done
  done

let rec turner (fight : State.fight) cur_x cur_y dir end_x end_y table =
  Render.render_square
    (Render.new_square
       (float_of_int cur_x /. float_of_int Magic_numbers.x_length *. 2.)
       (float_of_int cur_y /. float_of_int Magic_numbers.x_length *. 2.)
       Magic_numbers.width Magic_numbers.height "./path.png");
  Hashtbl.add table (cur_x, cur_y) true;
  if cur_x = end_x && cur_y = end_y then fight.spiraled <- true
  else
    (* pause for a little while*)
    (* let delay = Sys.time () +. 0.01 in let rec wait d = if Sys.time
       () > d then print_string "true" else (); if Sys.time () > d then *)
    match dir with
    | Right ->
        if Hashtbl.find table (cur_x, cur_y - 1) = false then
          turner fight cur_x (cur_y - 1) Down end_x end_y table
        else turner fight (cur_x + 1) cur_y Right end_x end_y table
    | Down ->
        if Hashtbl.find table (cur_x - 1, cur_y) = false then
          turner fight (cur_x - 1) cur_y Left end_x end_y table
        else turner fight cur_x (cur_y - 1) Down end_x end_y table
    | Left ->
        if Hashtbl.find table (cur_x, cur_y + 1) = false then
          turner fight cur_x (cur_y + 1) Up end_x end_y table
        else turner fight (cur_x - 1) cur_y Left end_x end_y table
    | Up ->
        if Hashtbl.find table (cur_x + 1, cur_y) = false then
          turner fight (cur_x + 1) cur_y Right end_x end_y table
        else turner fight cur_x (cur_y + 1) Up end_x end_y table

(* else wait d in wait delay *)

let render_spiral fight x y =
  let middle_x = (x - 1) / 2 in
  let middle_y = (y - 1) / 2 in
  let spiral_table = Hashtbl.create (x * y) in
  instantiate_dungeon_cells x y spiral_table;
  turner fight middle_x middle_y Up (x - 1) (y - 1) spiral_table;
  print_string "spiral done"
