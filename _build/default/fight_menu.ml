let render_menu
    ({
       action;
       attacking : bool;
       monster : Dungeon.monster;
       monster_string : string;
       input_string : string;
     } :
      State.fight) =
  Render.render_square
    (Render.new_square
       (1. /. float_of_int 3 *. 2.)
       (1.25 /. float_of_int 3 *. 2.)
       (Magic_numbers.width *. 4.)
       (Magic_numbers.height *. 4.)
       monster.sprite);
  if attacking then
    Font.render_font
      (Font.new_font monster_string 0.8 0.8 Magic_numbers.width
         Magic_numbers.height);
  Font.render_font
    (Font.new_font input_string 0. 0.5 Magic_numbers.width
       Magic_numbers.height);
  match action with
  | Attack ->
      Font.render_font
        (Font.new_font ">fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Recover ->
      Font.render_font
        (Font.new_font "fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font ">duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Run ->
      Font.render_font
        (Font.new_font "fuck" 0. 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "duck" 0.75 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font ">cuck" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)
