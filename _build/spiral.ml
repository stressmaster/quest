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

let rec turner
    (fight : State.fight)
    number_done
    number_needed
    cur_x
    cur_y
    dir
    end_x
    end_y
    table =
  Render.render_square
    (Render.new_square
       (float_of_int cur_x /. float_of_int Magic_numbers.x_length *. 2.)
       (float_of_int cur_y /. float_of_int Magic_numbers.x_length *. 2.)
       Magic_numbers.width Magic_numbers.height "./darkness.png");
  Hashtbl.add table (cur_x, cur_y) true;
  if cur_x = end_x && cur_y = end_y then fight.spiraled <- true
  else fight.spiraled <- false;
  if number_done = number_needed || fight.spiraled = true then ()
  else
    (* pause for a little while*)
    match dir with
    | Right ->
        if Hashtbl.find table (cur_x, cur_y - 1) = false then
          turner fight (number_done + 1) number_needed cur_x (cur_y - 1)
            Down end_x end_y table
        else
          turner fight (number_done + 1) number_needed (cur_x + 1) cur_y
            Right end_x end_y table
    | Down ->
        if Hashtbl.find table (cur_x - 1, cur_y) = false then
          turner fight (number_done + 1) number_needed (cur_x - 1) cur_y
            Left end_x end_y table
        else
          turner fight (number_done + 1) number_needed cur_x (cur_y - 1)
            Down end_x end_y table
    | Left ->
        if Hashtbl.find table (cur_x, cur_y + 1) = false then
          turner fight (number_done + 1) number_needed cur_x (cur_y + 1)
            Up end_x end_y table
        else
          turner fight (number_done + 1) number_needed (cur_x - 1) cur_y
            Left end_x end_y table
    | Up ->
        if Hashtbl.find table (cur_x + 1, cur_y) = false then
          turner fight (number_done + 1) number_needed (cur_x + 1) cur_y
            Right end_x end_y table
        else
          turner fight (number_done + 1) number_needed cur_x (cur_y + 1)
            Up end_x end_y table

let render_spiral fight x y =
  let middle_x = (x - 1) / 2 in
  let middle_y = (y - 1) / 2 in
  let spiral_table = Hashtbl.create (x * y) in
  instantiate_dungeon_cells x y spiral_table;
  let endpoint = Timer.current_time () in
  turner fight 0 (5 * endpoint) middle_x middle_y Up (x - 1) (y - 1)
    spiral_table
