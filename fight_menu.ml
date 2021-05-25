let render_not_fighting_square (fight : State.fight) =
  Render.render_square
    (Render.new_square
       (1. /. 3. *. 2.)
       (1.25 /. 3. *. 2.)
       (Magic_numbers.width *. 4.)
       (Magic_numbers.height *. 4.)
       fight.monster.sprite)

let render_timer_and_attack (fight : State.fight) =
  Render.render_square
    (Render.new_square 0. 1.8
       ( 500.
       *. (Timer.current_time "general" |> float_of_int)
       /. (fight.typing_limit |> float_of_int) )
       Magic_numbers.height "./timer.png");
  Font.render_font
    (Font.new_font fight.monster_string 0. 0.3 Magic_numbers.width
       Magic_numbers.height)

let match_fight_action (fight : State.fight) =
  match fight.action with
  | Attack ->
      Font.render_font
        (Font.new_font ">" 0. 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Recover ->
      Font.render_font
        (Font.new_font ">" 0.78 0.2 Magic_numbers.width
           Magic_numbers.height)
  | Run ->
      Font.render_font
        (Font.new_font ">" 1.5 0.2 Magic_numbers.width
           Magic_numbers.height)

let render_not_attacking_fonts (fight : State.fight) =
  Font.render_font
    (Font.new_font "fight" 0.1 0.2 Magic_numbers.width
       Magic_numbers.height);
  Font.render_font
    (Font.new_font "heal" 0.88 0.2 Magic_numbers.width
       Magic_numbers.height);
  Font.render_font
    (Font.new_font "run" 1.6 0.2 Magic_numbers.width
       Magic_numbers.height);
  match_fight_action fight

let render_menu_options (fight : State.fight) =
  Font.render_font
    (Font.new_font fight.input_string 0. 0.1 Magic_numbers.width
       Magic_numbers.height);
  Font.render_font
    (Font.new_font
       ("hp " ^ string_of_int fight.monster_health)
       0.8 1.6 Magic_numbers.width Magic_numbers.height);
  Font.render_font
    (Font.new_font
       ("hp " ^ string_of_int fight.player_health)
       0.1 0.6 Magic_numbers.width Magic_numbers.height)

let render_fight (fight : State.fight) =
  render_not_fighting_square fight;
  render_menu_options fight;
  if fight.attacking then render_timer_and_attack fight
  else render_not_attacking_fonts fight

let render_menu (fight : State.fight) =
  if fight.player_health <= 0 then
    Font.render_font
      (Font.new_font "you lose" 0.6 1. Magic_numbers.width
         Magic_numbers.height)
  else if not (fight.monster_health = 0) then render_fight fight
  else
    Font.render_font
      (Font.new_font "you win" 0.6499 1. Magic_numbers.width
         Magic_numbers.height)

let render_attack (fight : State.fight) =
  let monster_sprite =
    Render.new_square
      (1. /. float_of_int 3 *. 2.)
      (1.25 /. float_of_int 3 *. 2.)
      (Magic_numbers.width *. 4.)
      (Magic_numbers.height *. 4.)
      fight.monster.sprite
  in
  let darkness_sprite =
    Render.new_square
      (1. /. float_of_int 3 *. 2.)
      (1.25 /. float_of_int 3 *. 2.)
      (Magic_numbers.width *. 4.)
      (Magic_numbers.height *. 4.)
      "./darkness.png"
  in
  Render.render_square_flashes monster_sprite darkness_sprite 15

let render_player_damage () = Render.render_screen_shake 18
