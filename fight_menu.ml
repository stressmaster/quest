type action = State.action

let render_menu (a : action) =
  match a with
  | Run ->
      Render.render_square
        (Render.new_square
           (float_of_int 2 /. float_of_int 3 *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./path.png")
  | Recover ->
      Render.render_square
        (Render.new_square
           (float_of_int 1 /. float_of_int 3 *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./wall.png")
  | Attack ->
      Render.render_square
        (Render.new_square
           (float_of_int 0 /. float_of_int 3 *. 2.)
           (0.5 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./player.png")
