(*mutable spiraled : bool; mutable action : action; mutable attacking :
  bool; mutable monster : Dungeon.monster; mutable monster_string :
  string; mutable input_string : string;*)

(*({ spiraled; action; attacking : bool; monster : Dungeon.monster;
  monster_string : string; input_string : string; } : State.fight)*)

let render_menu (fight : State.fight) =
  Render.render_square
    (Render.new_square
       (1. /. float_of_int 3 *. 2.)
       (1.25 /. float_of_int 3 *. 2.)
       (Magic_numbers.width *. 4.)
       (Magic_numbers.height *. 4.)
       fight.monster.sprite);
  if fight.spiraled = false then
    Spiral.render_spiral fight Magic_numbers.x_length
      Magic_numbers.y_length
  else ()

(* if fight.attacking then Font.render_font (Font.new_font
   fight.monster_string 0.8 0.8 Magic_numbers.width
   Magic_numbers.height); Font.render_font (Font.new_font
   fight.input_string 0. 0.5 Magic_numbers.width Magic_numbers.height);
   Font.render_font (Font.new_font "fight" 0.1 0.2 Magic_numbers.width
   Magic_numbers.height); Font.render_font (Font.new_font "heal" 0.88
   0.2 Magic_numbers.width Magic_numbers.height); Font.render_font
   (Font.new_font "run" 1.6 0.2 Magic_numbers.width
   Magic_numbers.height); Font.render_font (Font.new_font
   (Timer.current_time () |> int_of_float |> Int.abs |> string_of_int)
   1. 1.6 Magic_numbers.width Magic_numbers.height); match fight.action
   with | Attack -> Font.render_font (Font.new_font ">" 0. 0.2
   Magic_numbers.width Magic_numbers.height) | Recover ->
   Font.render_font (Font.new_font ">" 0.78 0.2 Magic_numbers.width
   Magic_numbers.height) | Run -> Font.render_font (Font.new_font ">"
   1.5 0.2 Magic_numbers.width Magic_numbers.height) *)
