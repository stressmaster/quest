let render_menu
    ({ action; attacking : bool; monster : Dungeon.monster } :
      State.fight) =
  (* print_string monster.sprite; *)
  Render.render_square
    (Render.new_square
       (1. /. float_of_int 3 *. 2.)
       (1.25 /. float_of_int 3 *. 2.)
       (Magic_numbers.width *. 4.)
       (Magic_numbers.height *. 4.)
       monster.sprite);

  match action with
  | Run ->
      Render.render_square
        (Render.new_square
           ((float_of_int Magic_numbers.x_length -. 2.)
           /. float_of_int Magic_numbers.x_length
           *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./path.png")
  | Recover ->
      Render.render_square
        (Render.new_square
           ((float_of_int Magic_numbers.x_length -. 1.)
           /. 2.
           /. float_of_int Magic_numbers.x_length
           *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./wall.png")
  | Attack ->
      Render.render_square
        (Render.new_square
           (1. /. float_of_int Magic_numbers.x_length *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./player.png")
