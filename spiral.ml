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

let rec turner (fight : State.fight) ndone nneeded cx cy dir ex ey tab =
  Render.render_square
    (Render.new_square
       (float_of_int cx /. float_of_int Magic_numbers.x_length *. 2.)
       (float_of_int cy /. float_of_int Magic_numbers.x_length *. 2.)
       Magic_numbers.width Magic_numbers.height "./darkness.png");
  Hashtbl.add tab (cx, cy) true;
  if cx = ex && cy = ey then (
    fight.spiraled <- true;
    Render_stack.stack_pop ();
    Render_stack.stack_push Render_stack.FightRender)
  else fight.spiraled <- false;
  if ndone = nneeded || fight.spiraled then ()
  else turner_helper fight dir cx cy ex ey ndone nneeded tab

and turner_helper fight dir cx cy end_x end_y ndone nneeded table =
  match dir with
  | Right -> turner_right cx cy table fight ndone nneeded end_x end_y
  | Down -> turner_down cx cy table fight ndone nneeded end_x end_y
  | Left -> turner_left cx cy table fight ndone nneeded end_x end_y
  | Up -> turner_up cx cy table fight ndone nneeded end_x end_y

and turner_right cx cy tab fight ndone nneeded end_x end_y =
  if Hashtbl.find tab (cx, cy - 1) then
    turner fight (ndone + 1) nneeded (cx + 1) cy Right end_x end_y tab
  else turner fight (ndone + 1) nneeded cx (cy - 1) Down end_x end_y tab

and turner_left cx cy tab fight ndone nneeded end_x end_y =
  if Hashtbl.find tab (cx, cy + 1) then
    turner fight (ndone + 1) nneeded (cx - 1) cy Left end_x end_y tab
  else turner fight (ndone + 1) nneeded cx (cy + 1) Up end_x end_y tab

and turner_up cx cy tab fight ndone nneeded end_x end_y =
  if Hashtbl.find tab (cx + 1, cy) then
    turner fight (ndone + 1) nneeded cx (cy + 1) Up end_x end_y tab
  else
    turner fight (ndone + 1) nneeded (cx + 1) cy Right end_x end_y tab

and turner_down cx cy tab fight ndone nneeded end_x end_y =
  if Hashtbl.find tab (cx - 1, cy) then
    turner fight (ndone + 1) nneeded cx (cy - 1) Down end_x end_y tab
  else turner fight (ndone + 1) nneeded (cx - 1) cy Left end_x end_y tab

let render_spiral fight x y =
  let middle_x = (x - 1) / 2 in
  let middle_y = (y - 1) / 2 in
  let spiral_table = Hashtbl.create (x * y) in
  instantiate_dungeon_cells x y spiral_table;
  let endpoint = Timer.current_time "general" in
  turner fight 0 (5 * endpoint) middle_x middle_y Up (x - 1) (y - 1)
    spiral_table
