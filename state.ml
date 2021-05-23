open Random

type action =
  | Run
  | Recover
  | Attack

type game_over_action =
  | Quit
  | Revive
  | Restart

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
}

let curr_room c = c.room

let in_fight c = c.in_fight

let curr_fight c = c.fight

let curr_game_over c = c.game_over

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
    depth = 0;
    current_exp = 0;
    exp_bound = 10;
    current_weapon = Item.empty_item;
    current_armor = Item.empty_item;
    game_over = Quit;
  }

let init_state file_name =
  let game = Yojson.Basic.from_file file_name |> Game.from_json in
  let room = game |> Game.start_room in
  let monster =
    room |> Dungeon.get_magic_numbers |> Monsters.get_monster
  in
  Magic_numbers.update (Dungeon.get_magic_numbers room);
  init_current game room monster

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
  Audio.change_music "./camlished.wav"

let encounter bound = Random.int bound = 0

let is_in_fight current =
  let current_bound = Dungeon.get_bound current.room in
  (not
     ( current.location = current.room_exit
     || current.location = Dungeon.get_start current.room ))
  && encounter current_bound

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let change_to_next_room current =
  current.room <- Game.next_dungeon current.game current.room;
  current.game <- Game.add_to_game current.game current.room;
  current.location <- Dungeon.get_start current.room;
  current.room_exit <- Dungeon.get_exit current.room;
  current.depth <- current.depth + 1;
  let new_magic_numbers = current.room |> Dungeon.get_magic_numbers in
  Magic_numbers.update new_magic_numbers;
  Spriteanimation.init_animations new_magic_numbers.animations

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
  then change_to_next_room current
  else if current.room_exit = current.location then
    change_to_previous_room current

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
  current.current_armor <- Armor armor

let manage_weapon current x y weapon =
  if current.current_weapon <> NoItem then
    Dungeon.drop_item current.room (x, y) (Some current.current_weapon);
  current.current_weapon <- Weapon weapon

let manage_no_item current x y =
  if current.current_weapon <> NoItem then (
    Dungeon.drop_item current.room (x, y) (Some current.current_weapon);
    current.current_weapon <- NoItem )
  else if current.current_armor <> NoItem then (
    Dungeon.drop_item current.room (x, y) (Some current.current_armor);
    current.current_armor <- NoItem )

let manage_item current x y = function
  | Some (Item.Armor a) ->
      manage_armor current x y a;
      current.current_armor <- Armor a
  | Some (Item.Weapon w) ->
      manage_weapon current x y w;
      current.current_weapon <- Weapon w
  | _ -> manage_no_item current x y

(* [move current key] assigns a location to [current] based on [key]*)
let move current key =
  let x, y = current.location in
  current.in_fight <- is_in_fight current;
  if current.in_fight then (
    Audio.change_music "./unravel.wav";
    Render_stack.stack_push Render_stack.SpiralRender;
    Timer.reset_timer () );
  (* delete light right below when spiral works. it is a work around*)
  begin
    match key with
    | Glut.KEY_RIGHT | Glut.KEY_LEFT | Glut.KEY_UP | Glut.KEY_DOWN ->
        map_move current key x y
    | Glut.KEY_F2 ->
        manage_item current x y (Dungeon.get_item current.room (x, y))
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
    let our_rand = Random.int 36 in
    let key =
      if our_rand < 10 then char_of_int (our_rand + 48)
      else char_of_int (our_rand + 87)
    in
    random_string (length - 1) (Char.escaped key ^ acc)

let manage_damage mon_HP current =
  if mon_HP > Monsters.get_monster_max_HP current.fight.monster / 3 then
    Monsters.get_monster_string current.fight.monster
  else random_string (Random.int 10) ""

let take_damage mon_HP current =
  Render_stack.stack_push Render_stack.ScreenshakeRender;
  current.fight.player_health <-
    max 0 (current.fight.player_health - max 1 (mon_HP / 20))

let manage_attack mon_str mon_HP diff current =
  let damage = max (String.length mon_str - diff) 0 in
  current.fight.monster_health <- max (mon_HP - damage) 0;
  if current.fight.monster_health > 0 then take_damage mon_HP current;
  if damage > 0 then Render_stack.stack_push Render_stack.AttackRender;
  Audio.play_sound "./oof.wav"

let manage_recover mon_str mon_HP diff current =
  if current.fight.monster_health > 0 then take_damage mon_HP current;
  current.fight.player_health <-
    (let healing = max (String.length mon_str - diff) 0 in
     min (current.fight.player_health + healing) current.health)

let manage_run str mon_str mon_HP diff current =
  if diff > String.length str / 3 && current.fight.monster_health > 0
  then take_damage mon_HP current;
  if diff <= String.length str / 3 then (
    Render_stack.stack_pop ();
    reset_fight current )

let enter_case str mon_str mon_HP current =
  current.fight.monster_string <- manage_damage mon_HP current;
  let diff = Levenshtein.dist str mon_str in
  ( match current.fight.action with
  | Attack -> manage_attack mon_str mon_HP diff current
  | Recover -> manage_recover mon_str mon_HP diff current
  | Run -> manage_run str mon_str mon_HP diff current );
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

let gaming_move current key =
  match current.game_over with
  | Quit when key = 13 ->
      Game.update_file
        (Game.json_maker true
           (Game.game_depth current.game)
           current.game);
      ignore (exit 0);
      current
  | Revive when key = 13 ->
      Render_stack.stack_pop ();
      current
  | Restart when key = 13 -> current
  (* reset save.json*)
  | _ -> current

let clamp_str str = String.sub str 0 (min (String.length str) 20)

let typing_move current key =
  match Render_stack.stack_peek () with
  | FightRender when current.fight.attacking ->
      current.fight.input_string <-
        clamp_str (fighting_case current key);
      current
  | GameoverRender -> gaming_move current key
  | _ -> current

let menu_move current key =
  ( match key with
  | Glut.KEY_RIGHT ->
      current.fight.action <- get_next_action current.fight.action
  | Glut.KEY_LEFT ->
      current.fight.action <- get_prev_action current.fight.action
  | Glut.KEY_DOWN ->
      Render_stack.stack_pop ();
      reset_fight current
  | Glut.KEY_UP when not current.fight.attacking ->
      Timer.reset_timer ();
      current.fight.attacking <- not current.fight.attacking
  | _ -> () );
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

(* [controller current key] updates the [current] based on [key]*)

let level_up current exp =
  current.current_exp <- current.current_exp + exp;
  while current.current_exp > current.exp_bound do
    current.level <- current.level + 1;
    current.current_exp <- current.current_exp - current.exp_bound;
    current.exp_bound <- current.level * current.exp_bound;
    ()
  done

let manage_exp current =
  level_up current 50;
  Render_stack.stack_pop ();
  reset_fight current;
  current

let manage_game_over current =
  Render_stack.stack_pop ();
  Render_stack.stack_push GameoverRender;
  reset_fight current;
  current

let controller current key =
  (* move after end of fight*)
  let top = Render_stack.stack_peek () in
  match top with
  | DungeonRender -> move current key
  | FightRender when current.fight.monster_health = 0 ->
      manage_exp current
  | FightRender when current.fight.player_health = 0 ->
      manage_game_over current
  | FightRender -> menu_move current key
  | GameoverRender -> game_over_move current key
  | _ -> current

(* (* move after end of fight*) if current.in_fight &&
   current.fight.monster_health = 0 then ( Render_stack.stack_pop ();
   reset_fight current; current (* can't move during spiral animation*)
   ) else if current.in_fight && not current.fight.spiraled then current
   (* menu move*) else if current.in_fight && not
   current.fight.attacking then menu_move current key (* move around
   map*) else if not current.in_fight then move current key else current *)

let render_inventory c =
  Render.render_square
    (Render.new_square 1.4 0.1
       (!Magic_numbers.get_magic.width *. 1.3)
       (!Magic_numbers.get_magic.height *. 1.3)
       (Item.get_item_sprite c.current_weapon));
  Font.render_font
    (Font.new_font
       (string_of_int (Item.get_item_modifier c.current_weapon))
       1.56 0.12
       (!Magic_numbers.get_magic.width *. 0.5)
       (!Magic_numbers.get_magic.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name c.current_weapon)
       0. 0.
       (!Magic_numbers.get_magic.width *. 0.5)
       (!Magic_numbers.get_magic.height *. 0.5));
  Render.render_square
    (Render.new_square 1.7 0.1
       (!Magic_numbers.get_magic.width *. 1.3)
       (!Magic_numbers.get_magic.height *. 1.3)
       (Item.get_item_sprite c.current_armor));
  Font.render_font
    (Font.new_font
       (string_of_int (Item.get_item_modifier c.current_armor))
       1.86 0.12
       (!Magic_numbers.get_magic.width *. 0.5)
       (!Magic_numbers.get_magic.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name c.current_armor)
       0. 0.05
       (!Magic_numbers.get_magic.width *. 0.5)
       (!Magic_numbers.get_magic.height *. 0.5))

let check_time_limit current =
  if
    current.fight.attacking
    && Timer.current_time () > current.fight.typing_limit
  then typing_move current 13
  else current
