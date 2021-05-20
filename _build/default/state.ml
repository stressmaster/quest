open Random

type action =
  | Run
  | Recover
  | Attack

type fight = {
  mutable spiraled : bool;
  mutable action : action;
  mutable attacking : bool;
  mutable monster : Dungeon.monster;
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

type current = {
  mutable game : Game.t;
  mutable location : int * int;
  mutable room : Dungeon.t;
  mutable room_exit : int * int;
  mutable in_fight : bool;
  mutable fight : fight;
  mutable health : int;
  mutable level : int;
  mutable current_exp : int;
  mutable exp_bound : int;
  mutable current_weapon : Item.t;
  mutable current_armor : Item.t;
}

let curr_room c = c.room

let in_fight c = c.in_fight

let curr_fight c = c.fight

let init_state file_name =
  let g = Yojson.Basic.from_file file_name |> Game.from_json in
  let r = g |> Game.start_room in
  let m = Dungeon.get_monster r in
  {
    game = g;
    room = r;
    room_exit = r |> Dungeon.get_exit;
    location = r |> Dungeon.get_start;
    in_fight = false;
    fight =
      {
        spiraled = false;
        action = Attack;
        attacking = false;
        monster = m;
        monster_string = Dungeon.get_monster_string m;
        monster_health = Dungeon.get_monster_HP m;
        player_health = Magic_numbers.health;
        typing_limit =
          m |> Dungeon.get_monster_string |> String.length |> ( * ) 10;
        input_string = "";
      };
    health = Magic_numbers.health;
    level = 1;
    current_exp = 0;
    exp_bound = 10;
    current_weapon = Item.empty_item;
    current_armor = Item.empty_item;
  }

let reset_fight c =
  let new_m = Dungeon.get_monster c.room in
  c.in_fight <- false;
  c.fight.spiraled <- false;
  c.fight.action <- Attack;
  c.fight.attacking <- false;
  c.fight.monster <- new_m;
  c.fight.monster_string <- Dungeon.get_monster_string new_m;
  c.fight.monster_health <- Dungeon.get_monster_HP new_m;
  c.fight.player_health <- c.health;
  Audio.change_music "./camlished.wav"

let fight_decision bound = Random.int bound = 0

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let map_move current key =
  let current_bound = Dungeon.get_bound current.room in
  let x, y = current.location in
  current.in_fight <-
    (if
     current.location = current.room_exit
     || current.location = Dungeon.get_start current.room
    then false
    else fight_decision current_bound);
  if current.in_fight then (
    Audio.change_music "./unravel.wav";
    Render_stack.stack_push Render_stack.SpiralRender);
  (* delete light right below when spiral works. it is a work around*)
  begin
    match key with
    | Glut.KEY_RIGHT ->
        current.location <-
          (if Dungeon.is_wall current.room (x + 1, y) then (x, y)
          else (x + 1, y))
    | Glut.KEY_LEFT ->
        current.location <-
          (if Dungeon.is_wall current.room (x - 1, y) then (x, y)
          else (x - 1, y))
    | Glut.KEY_UP ->
        current.location <-
          (if Dungeon.is_wall current.room (x, y + 1) then (x, y)
          else (x, y + 1))
    | Glut.KEY_DOWN ->
        current.location <-
          (if Dungeon.is_wall current.room (x, y - 1) then (x, y)
          else (x, y - 1))
    | Glut.KEY_F2 -> (
        let i = Dungeon.get_item current.room (x, y) in
        match i with
        | Some (Armor a) -> current.current_armor <- Armor a
        | Some (Weapon w) -> current.current_weapon <- Weapon w
        | _ -> ())
    | _ -> ()
  end;
  if current.in_fight then Timer.reset_timer ();
  let should_change_next = current.room_exit = current.location in
  let should_change_prev =
    if Game.start_room current.game = current.room then false
    else current.location = Dungeon.get_start current.room
  in
  if should_change_next then (
    current.room <- Game.next_dungeon current.game current.room;
    current.game <- Game.add_to_game current.game current.room;
    current.location <- Dungeon.get_start current.room;
    current.room_exit <- Dungeon.get_exit current.room)
  else if should_change_prev then (
    current.room <- Game.prev_dungeon current.game current.room;
    current.location <- Dungeon.get_exit current.room;
    current.room_exit <- Dungeon.get_exit current.room);
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

let typing_case current key =
  let str = current.fight.input_string
  and mon_str = current.fight.monster_string
  and mon_HP = current.fight.monster_health in
  if key = 13 then (
    current.fight.monster_string <-
      (if mon_HP > 15 then
       Dungeon.get_monster_string current.fight.monster
      else random_string (Random.int 15) "");
    let diff = Levenshtein.dist str mon_str in
    (match current.fight.action with
    | Attack ->
        let damage = max (String.length mon_str - diff) 0 in
        current.fight.monster_health <- max (mon_HP - damage) 0;
        if current.fight.monster_health > 0 then (
          Render_stack.stack_push Render_stack.ScreenshakeRender;
          current.fight.player_health <-
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)));
        (* Font.render_font (Font.new_font ("Monster used " ^
           Dungeon.monster_move current.fight.monster ^ "!") 0. 0.3
           Magic_numbers.width Magic_numbers.height); *)
        if damage > 0 then
          Render_stack.stack_push Render_stack.AttackRender;
        Audio.play_sound "./oof.wav"
    | Recover ->
        if current.fight.monster_health > 0 then (
          Render_stack.stack_push Render_stack.ScreenshakeRender;
          current.fight.player_health <-
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)));
        current.fight.player_health <-
          (let healing = max (String.length mon_str - diff) 0 in
           min (current.fight.player_health + healing) current.health)
    | Run ->
        if
          diff > String.length str / 3
          && current.fight.monster_health > 0
        then (
          Render_stack.stack_push Render_stack.ScreenshakeRender;
          current.fight.player_health <-
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)));
        if diff <= String.length str / 3 then (
          Render_stack.stack_pop ();
          reset_fight current));

    current.fight.attacking <- false;
    "")
  else if key = 127 then
    String.sub str 0 (max (String.length str - 1) 0)
  else if is_typable key then str ^ Char.escaped (Char.chr key)
  else str

let clamp_str str = String.sub str 0 (min (String.length str) 20)

let typing_move current key =
  if current.in_fight && current.fight.attacking then
    current.fight.input_string <- clamp_str (typing_case current key);
  current

let menu_move current key =
  (match key with
  | Glut.KEY_RIGHT ->
      current.fight.action <- get_next_action current.fight.action
  | Glut.KEY_LEFT ->
      current.fight.action <- get_prev_action current.fight.action
  | Glut.KEY_DOWN ->
      Render_stack.stack_pop ();
      reset_fight current
  | Glut.KEY_UP ->
      if not current.fight.attacking then (
        Timer.reset_timer ();
        current.fight.attacking <- not current.fight.attacking)
  | _ -> ());
  current

(* [controller current key] updates the [current] based on [key]*)

let manage_exp current exp =
  current.current_exp <- current.current_exp + exp;
  while current.current_exp > current.exp_bound do
    current.level <- current.level + 1;
    current.current_exp <- current.current_exp - current.exp_bound;
    current.exp_bound <- current.level * current.exp_bound;
    ()
  done

let controller current key =
  (* move after end of fight*)
  let top = Render_stack.stack_peek () in
  match top with
  | Render_stack.DungeonRender -> map_move current key
  | Render_stack.FightRender ->
      if current.fight.monster_health = 0 then (
        manage_exp current 50;
        Render_stack.stack_pop ();
        reset_fight current;
        current)
      else menu_move current key
  | Render_stack.SpiralRender -> current
  | Render_stack.AttackRender -> current
  | Render_stack.ScreenshakeRender -> current

(* (* move after end of fight*) if current.in_fight &&
   current.fight.monster_health = 0 then ( Render_stack.stack_pop ();
   reset_fight current; current (* can't move during spiral animation*)
   ) else if current.in_fight && not current.fight.spiraled then current
   (* menu move*) else if current.in_fight && not
   current.fight.attacking then menu_move current key (* move around
   map*) else if not current.in_fight then map_move current key else
   current *)

let render_inventory c =
  Render.render_square
    (Render.new_square 1.4 0.1
       (Magic_numbers.width *. 1.3)
       (Magic_numbers.height *. 1.3)
       (Item.get_item_sprite c.current_weapon));
  Font.render_font
    (Font.new_font
       (string_of_int (Item.get_item_modifier c.current_weapon))
       1.56 0.12
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name c.current_weapon)
       0. 0.
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Render.render_square
    (Render.new_square 1.7 0.1
       (Magic_numbers.width *. 1.3)
       (Magic_numbers.height *. 1.3)
       (Item.get_item_sprite c.current_armor));
  Font.render_font
    (Font.new_font
       (string_of_int (Item.get_item_modifier c.current_armor))
       1.86 0.12
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5));
  Font.render_font ~spacing:0.05
    (Font.new_font
       (Item.get_item_name c.current_armor)
       0. 0.05
       (Magic_numbers.width *. 0.5)
       (Magic_numbers.height *. 0.5))

let check_time_limit current =
  if
    current.fight.attacking
    && Timer.current_time () > current.fight.typing_limit
  then typing_move current 13
  else current
