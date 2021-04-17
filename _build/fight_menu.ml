type action = State.action

let render_menu (a : action) =
  match a with
  | Run ->
      Render.render_square
        (Render.new_square
           (float_of_int 0 /. float_of_int 3 *. 2.)
           (float_of_int 0 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./player.png")
  | Recover ->
      Render.render_square
        (Render.new_square
           (float_of_int 1 /. float_of_int 3 *. 2.)
           (float_of_int 1 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./player.png")
  | Attack ->
      Render.render_square
        (Render.new_square
           (float_of_int 2 /. float_of_int 3 *. 2.)
           (float_of_int 2 /. float_of_int 3 *. 2.)
           Magic_numbers.width Magic_numbers.height "./player.png")
