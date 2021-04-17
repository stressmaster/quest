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
      Font.render_font
        (Font.new_font ">fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Attack ->
      Font.render_font
        (Font.new_font "fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font ">duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Recover ->
      Font.render_font
        (Font.new_font "fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font ">cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
