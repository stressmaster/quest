open Random

type action =
  | Run
  | Recover
  | Attack

type game_over_action =
  | Quit
  | Revive
  | Restart

type start_menu_action =
  | NewGame
  | Continue

type fight = {
  mutable spiraled : bool;
  mutable action : action;
  mutable attacking : bool;
  mutable monster : Monsters.t;
  mutable monster_string : string;
  mutable monster_health : int;
  mutable player_health : int;
  mutable typing_limit : int;
  mutable input_string : string;
}

let get_next_action = function
  | Run -> Attack
  | Attack -> Recover
  | Recover -> Run

let get_prev_action = function
  | Run -> Recover
  | Recover -> Attack
  | Attack -> Run

let get_next_game_over = function
  | Quit -> Revive
  | Revive -> Restart
  | Restart -> Quit

let get_prev_game_over = function
  | Quit -> Restart
  | Revive -> Quit
  | Restart -> Revive

let get_next_start_menu = function
  | NewGame -> Continue
  | Continue -> NewGame

type current = {
  mutable game : Game.t;
  mutable location : int * int;
  mutable room : Dungeon.t;
  mutable room_exit : int * int;
  mutable in_fight : bool;
  mutable fight : fight;
  mutable health : int;
  mutable level : int;
  mutable depth : int;
  mutable current_exp : int;
  mutable exp_bound : int;
  mutable current_weapon : Item.t;
  mutable current_armor : Item.t;
  mutable game_over : game_over_action;
  mutable start_menu : start_menu_action;
  mutable lives : int;
}

let curr_room c = c.room

let in_fight c = c.in_fight

let curr_fight c = c.fight

let curr_game_over c = c.game_over

let curr_start_menu c = c.start_menu

let curr_lives c = c.lives

let init_fight monster =
  {
    spiraled = false;
    action = Attack;
    attacking = false;
    monster;
    monster_string = Monsters.get_monster_string monster;
    monster_health = Monsters.get_monster_HP monster;
    player_health = !Magic_numbers.get_magic.health;
    typing_limit =
      monster |> Monsters.get_monster_string |> String.length
      |> ( * ) 10;
    input_string = "";
  }

let init_current game room monster =
  {
    game;
    room;
    room_exit = room |> Dungeon.get_exit;
    location = room |> Dungeon.get_start;
    in_fight = false;
    fight = init_fight monster;
    health = !Magic_numbers.get_magic.health;
    level = 1;
    depth = 1;
    current_exp = 0;
    exp_bound = 5;
    current_weapon = Item.empty_item;
    current_armor = Item.empty_item;
    game_over = Quit;
    start_menu = NewGame;
    lives = 3;
  }

let init_state file_name =
  let game = Yojson.Basic.from_file file_name |> Game.from_json in
  let room = game |> Game.start_room in
  let monster =
    room |> Dungeon.get_magic_numbers |> Monsters.get_monster
  in
  Magic_numbers.update (Dungeon.get_magic_numbers room);
  init_current game room monster

let json_mem = Yojson.Basic.Util.member

let json_int = Yojson.Basic.Util.to_int

let init_fight_json monster =
  {
    spiraled = false;
    action = Attack;
    attacking = false;
    monster;
    monster_string = Monsters.get_monster_string monster;
    monster_health = Monsters.get_monster_HP monster;
    player_health = !Magic_numbers.get_magic.health;
    typing_limit =
      monster |> Monsters.get_monster_string |> String.length
      |> ( * ) 10;
    input_string = "";
  }

let init_current_json ourjson game room monster =
  {
    game;
    room;
    room_exit = room |> Dungeon.get_exit;
    location =
      ( ourjson |> json_mem "locationx" |> json_int,
        ourjson |> json_mem "locationy" |> json_int );
    in_fight = false;
    fight = init_fight_json monster;
    health = !Magic_numbers.get_magic.health;
    level = ourjson |> json_mem "level" |> json_int;
    depth = ourjson |> json_mem "number_rooms" |> json_int;
    current_exp = ourjson |> json_mem "current_exp" |> json_int;
    exp_bound = ourjson |> json_mem "exp_bound" |> json_int;
    current_weapon =
      ourjson |> json_mem "current_weapon" |> Game.item_of_json;
    current_armor =
      ourjson |> json_mem "current_armor" |> Game.item_of_json;
    game_over = Quit;
    start_menu = NewGame;
    lives = ourjson |> json_mem "lives" |> json_int;
  }

let init_state_from_save file_name =
  let ourjson = Yojson.Basic.from_file file_name in
  let game = ourjson |> Game.save_json in
  let current_room_id = ourjson |> json_mem "current_id" |> json_int in
  let room = Game.nth_room game current_room_id in
  let monster =
    room |> Dungeon.get_magic_numbers |> Monsters.get_monster
  in
  Magic_numbers.update (Dungeon.get_magic_numbers room);
  init_current_json ourjson game room monster

let reset_fight c =
  let new_m =
    c.room |> Dungeon.get_magic_numbers |> Monsters.get_monster
  in
  c.in_fight <- false;
  c.fight.spiraled <- false;
  c.fight.action <- Attack;
  c.fight.attacking <- false;
  c.fight.monster <- new_m;
  c.fight.monster_string <- Monsters.get_monster_string new_m;
  c.fight.monster_health <- Monsters.get_monster_HP new_m;
  c.fight.player_health <- c.health;
  match Render_stack.stack_peek () with
  | GameoverRender -> Audio.change_music "./unravel.wav"
  | _ -> Audio.change_music "./camlished.wav"

let encounter bound = Random.int bound = 0

let is_in_fight current =
  let current_bound = Dungeon.get_bound current.room in
  (not
     (current.location = current.room_exit
     || current.location = Dungeon.get_start current.room))
  && encounter current_bound

let player_loc state = state.location

let cramp_wall current loc1 loc2 =
  current.location <-
    (if Dungeon.is_wall current.room loc1 then loc2 else loc1)

let map_move current key x y =
  match key with
  | Glut.KEY_RIGHT -> cramp_wall current (x + 1, y) (x, y)
  | Glut.KEY_LEFT -> cramp_wall current (x - 1, y) (x, y)
  | Glut.KEY_UP -> cramp_wall current (x, y + 1) (x, y)
  | Glut.KEY_DOWN -> cramp_wall current (x, y - 1) (x, y)
  | _ -> ()

let manage_armor current x y armor =
  if current.current_armor <> NoItem then
    Dungeon.drop_item current.room (x, y) (Some current.current_armor);
  current.current_armor <- Armor armor;
  current.health <-
    !Magic_numbers.get_magic.health
    + Item.get_item_modifier (Armor armor)

let manage_weapon current x y weapon =
  if current.current_weapon <> NoItem then
    Dungeon.drop_item current.room (x, y) (Some current.current_weapon);
  current.current_weapon <- Weapon weapon

let manage_no_item current x y =
  if current.current_weapon <> NoItem then (
    Dungeon.drop_item current.room (x, y) (Some current.current_weapon);
    current.current_weapon <- NoItem)
  else if current.current_armor <> NoItem then (
    Dungeon.drop_item current.room (x, y) (Some current.current_armor);
    current.current_armor <- NoItem)

let manage_item current x y item =
  match item with
  | Some (Item.Armor a) ->
      manage_armor current x y a;
      current.current_armor <- Armor a
  | Some (Item.Weapon w) ->
      manage_weapon current x y w;
      current.current_weapon <- Weapon w
  | _ -> manage_no_item current x y

let reach_end current =
  if current.level >= 20 && current.level >= 10 then (
    Render_stack.stack_push WinRender;
    Audio.change_music "./clarksonvoice.wav")

(* [move current key] assigns a location to [current] based on [key]*)
let change_to_next_room current =
  current.room <- Game.next_dungeon current.game current.room;
  current.game <- Game.add_to_game current.game current.room;
  current.location <- Dungeon.get_start current.room;
  current.room_exit <- Dungeon.get_exit current.room;
  current.depth <- current.depth + 1;
  let new_magic_numbers = current.room |> Dungeon.get_magic_numbers in
  Magic_numbers.update new_magic_numbers;
  Spriteanimation.init_animations new_magic_numbers.animations;
  reach_end current

let change_to_previous_room current =
  current.room <- Game.prev_dungeon current.game current.room;
  current.location <- Dungeon.get_exit current.room;
  current.room_exit <- Dungeon.get_exit current.room;
  current.depth <- current.depth - 1;
  let new_magic_numbers = current.room |> Dungeon.get_magic_numbers in
  Magic_numbers.update new_magic_numbers;
  Spriteanimation.init_animations new_magic_numbers.animations

let should_change_room current =
  if
    (not (current.depth = 0))
    && Dungeon.get_start current.room = current.location
  then change_to_previous_room current
  else if current.room_exit = current.location then
    change_to_next_room current

(* [move current key] assigns a location to [current] based on [key]*)
let move current key =
  let x, y = current.location in
  current.in_fight <- is_in_fight current;
  if current.in_fight then (
    current.fight.monster <-
      Monsters.change_monster_hp current.fight.monster
        (current.fight.monster.max_hp
        + int_of_float (1.5 *. float_of_int current.depth));
    current.fight.monster_health <-
      current.fight.monster_health
      + int_of_float (1.5 *. float_of_int current.depth);
    Audio.change_music "./camlished_battle.wav";
    Render_stack.stack_push Render_stack.SpiralRender;
    Timer.reset_timer "general");
  begin
    match key with
    | Glut.KEY_RIGHT | Glut.KEY_LEFT | Glut.KEY_UP | Glut.KEY_DOWN ->
        map_move current key x y
    | _ -> ()
  end;
  should_change_room current;
  current

let is_typable key =
  (48 <= key && key <= 57)
  || (65 <= key && key <= 90)
  || (97 <= key && key <= 122)
  || key = 32

let rec random_string length acc =
  if length = 0 then acc
  else
    let ourrand = Random.int 36 in
    let key =
      if ourrand < 10 then char_of_int (ourrand + 48)
      else char_of_int (ourrand + 87)
    in
    random_string (length - 1) (Char.escaped key ^ acc)

let manage_damage mon_HP current =
  if mon_HP > Monsters.get_monster_max_HP current.fight.monster / 3 then
    Monsters.get_monster_string current.fight.monster
  else
    let gibberish_length = min 15 (current.depth + 4) in
    random_string (1 + Random.int gibberish_length) ""

let take_damage mon_HP current =
  Render_stack.stack_push Render_stack.ScreenshakeRender;
  current.fight.player_health <-
    max 0 (current.fight.player_health - max 1 (mon_HP / 5))

let manage_attack mon_str mon_HP diff current =
  let damage = max (String.length mon_str - diff) 0 in
  let proportion_damage = damage / String.length mon_str in
  let dealt =
    if damage = 0 then 0
    else
      5
      + proportion_damage
        * Item.get_item_modifier current.current_weapon
  in
  current.fight.monster_health <- max (mon_HP - dealt) 0;
  if current.fight.monster_health > 0 then take_damage mon_HP current;
  if damage > 0 then Render_stack.stack_push Render_stack.AttackRender;
  Audio.play_sound "./oof.wav"

let manage_recover mon_str mon_HP diff current =
  if current.fight.monster_health > 0 then take_damage mon_HP current;
  current.fight.player_health <-
    (let healing = max (String.length mon_str - diff) 0 in
     let proportion_healing = healing / String.length mon_str in
     if proportion_healing = 0 then current.fight.player_health
     else
       min
         (6 + current.fight.player_health
         + (proportion_healing * current.fight.player_health / 2))
         current.health)

let manage_run str mon_str mon_HP diff current =
  if diff > String.length str / 3 && current.fight.monster_health > 0
  then take_damage mon_HP current;
  if diff <= String.length str / 3 then (
    Render_stack.stack_pop ();
    reset_fight current)

let enter_case str mon_str mon_HP current =
  current.fight.monster_string <- manage_damage mon_HP current;
  let diff = Levenshtein.dist str mon_str in
  (match current.fight.action with
  | Attack -> manage_attack mon_str mon_HP diff current
  | Recover -> manage_recover mon_str mon_HP diff current
  | Run -> manage_run str mon_str mon_HP diff current);
  current.fight.attacking <- false;
  ""

let fighting_case current key =
  let str = current.fight.input_string in
  match key with
  | 13 ->
      enter_case str current.fight.monster_string
        current.fight.monster_health current
  | 127 -> String.sub str 0 (max (String.length str - 1) 0)
  | _ when is_typable key -> str ^ Char.escaped (Char.chr key)
  | _ -> str

let pop_and_change_music () =
  Audio.change_music "./camlished.wav";
  Render_stack.stack_pop ()

let starting_move current key =
  let exists =
    Yojson.Basic.from_file "save.json"
    |> Yojson.Basic.Util.member "exists"
    |> Yojson.Basic.Util.to_bool
  in
  match current.start_menu with
  | NewGame when key = 13 ->
      pop_and_change_music ();
      Game.update_file Game.reset_save;
      current
  | Continue when key = 13 && exists ->
      pop_and_change_music ();
      init_state_from_save "save.json"
  | _ -> current

let save_game current =
  Game.update_file
    (Game.json_maker current.level current.health current.lives true
       (fst current.location) (snd current.location) current.depth
       (Game.game_depth current.game)
       current.current_exp current.exp_bound current.current_weapon
       current.current_armor current.game);
  ()

let gaming_move current key =
  match current.game_over with
  | Quit when key = 13 ->
      save_game current;
      ignore (exit 0);
      current
  | Revive when key = 13 && current.lives > 1 ->
      Audio.change_music "./camlished.wav";
      Render_stack.stack_pop ();
      current.lives <- current.lives - 1;
      current
  | Restart when key = 13 ->
      Audio.change_music "./camlished.wav";
      Timer.reset_timer "big";
      Render_stack.stack_push DungeonRender;
      Game.update_file Game.reset_save;
      init_state "sample_game.json"
  | _ -> current

let clamp_str str = String.sub str 0 (min (String.length str) 20)

let typing_move current key =
  let x, y = current.location in
  match Render_stack.stack_peek () with
  | DungeonRender when key = 115 ->
      save_game current;
      current
  | DungeonRender when key = 13 ->
      manage_item current x y (Dungeon.get_item current.room (x, y));
      current
  | FightRender when current.fight.attacking ->
      current.fight.input_string <-
        clamp_str (fighting_case current key);
      current
  | GameoverRender -> gaming_move current key
  | StartRender -> starting_move current key
  | _ -> current

let menu_move current key =
  (match key with
  | Glut.KEY_RIGHT ->
      current.fight.action <- get_next_action current.fight.action
  | Glut.KEY_LEFT ->
      current.fight.action <- get_prev_action current.fight.action
  | Glut.KEY_UP ->
      if not current.fight.attacking then (
        Timer.reset_timer "general";
        current.fight.attacking <- not current.fight.attacking)
  | _ -> ());
  current

let game_over_move current key =
  match key with
  | Glut.KEY_DOWN ->
      current.game_over <- get_next_game_over current.game_over;
      current
  | Glut.KEY_UP ->
      current.game_over <- get_prev_game_over current.game_over;
      current
  | _ -> current

let start_menu_move current key =
  match key with
  | Glut.KEY_DOWN | Glut.KEY_UP ->
      current.start_menu <- get_next_start_menu current.start_menu;
      current
  | _ -> current

(* [controller current key] updates the [current] based on [key]*)

let level_up current exp =
  current.current_exp <- current.current_exp + exp;
  while current.current_exp > current.exp_bound do
    current.level <- current.level + 1;
    current.current_exp <- current.current_exp - current.exp_bound;
    current.exp_bound <- current.level * 4;
    current.health <- current.health + (2 * current.level);
    ()
  done

let manage_exp current =
  level_up current (Dungeon.get_id current.room);
  Render_stack.stack_pop ();
  reset_fight current;
  current

let manage_game_over current =
  Render_stack.stack_pop ();
  Render_stack.stack_push GameoverRender;
  reset_fight current;
  current

let controller current key =
  let top = Render_stack.stack_peek () in
  match top with
  | DungeonRender -> move current key
  | FightRender when current.fight.monster_health = 0 ->
      manage_exp current
  | FightRender when current.fight.player_health = 0 ->
      manage_game_over current
  | FightRender -> menu_move current key
  | GameoverRender -> game_over_move current key
  | StartRender -> start_menu_move current key
  | _ -> current

let render_item_boxes current =
  Render.render_square
    (Render.new_square 1.4 0.1
       (Magic_numbers.width *. 1.3)
       (Magic_numbers.height *. 1.3)
       (Item.get_item_sprite current.current_weapon));
  Render.render_square
    (Render.new_square 1.7 0.1
       (Magic_numbers.width *. 1.3)
       (Magic_numbers.height *. 1.3)
       (Item.get_item_sprite current.current_armor))

let render_item_names current =
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name current.current_weapon)
       0. 0.
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name current.current_armor)
       0. 0.05
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5))

let render_modifier current =
  Font.render_font ~spacing:0.05
    (Font.new_font
       (string_of_int (Item.get_item_modifier current.current_weapon))
       1.56 0.12
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (string_of_int (Item.get_item_modifier current.current_armor))
       1.86 0.12
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5))

let render_inventory current =
  render_item_boxes current;
  render_item_names current;
  render_modifier current

let render_exp current =
  Font.render_font ~spacing:0.05
    (Font.new_font
       ("level " ^ string_of_int current.level)
       1.3 1.9
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       ("exp left "
       ^ string_of_int (current.exp_bound - current.current_exp))
       1.3 1.8
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5))

let check_time_limit current =
  if
    current.fight.attacking
    && Timer.current_time "general" > current.fight.typing_limit
  then typing_move current 13
  else current
