(*mutable spiraled : bool; mutable action : action; mutable attacking :
  bool; mutable monster : Dungeon.monster; mutable monster_string :
  string; mutable input_string : string;*)

(*({ spiraled; action; attacking : bool; monster : Dungeon.monster;
  monster_string : string; input_string : string; } : State.fight)*)

let render_menu (fight : State.fight) =
  if not (fight.monster_health = 0) then begin
    Render.render_square
      (Render.new_square
         (1. /. float_of_int 3 *. 2.)
         (1.25 /. float_of_int 3 *. 2.)
         (Magic_numbers.width *. 4.)
         (Magic_numbers.height *. 4.)
         fight.monster.sprite);
    if fight.attacking then (
      Render.render_square
        (Render.new_square 0. 1.8
           (500.
           *. (Timer.current_time () |> float_of_int)
           /. (fight.typing_limit |> float_of_int))
           Magic_numbers.height fight.monster.sprite);
      Font.render_font
        (Font.new_font fight.monster_string 0. 0.3 Magic_numbers.width
           Magic_numbers.height));
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
         0.1 0.6 Magic_numbers.width Magic_numbers.height);
    if not fight.attacking then (
      Font.render_font
        (Font.new_font "fight" 0.1 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "heal" 0.88 0.2 Magic_numbers.width
           Magic_numbers.height);
      Font.render_font
        (Font.new_font "run" 1.6 0.2 Magic_numbers.width
           Magic_numbers.height);
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
               Magic_numbers.height))
  end
  else
    Font.render_font
      (Font.new_font "you win" 0.6 1. Magic_numbers.width
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

let monster_move (fight : State.fight) =
  let ourstringlist = fight.monster.attack_strings in
  let ourlen = List.length ourstringlist in
  let ourint = Random.int ourlen in
  let rec listsearcher lst cur num =
    match lst with
    | [] -> failwith "wtf"
    | h :: t -> if cur == num then h else listsearcher t (cur + 1) num
  in
  listsearcher ourstringlist 0 ourint

let render_player_damage () = Render.render_screen_shake 18
