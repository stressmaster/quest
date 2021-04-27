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
  game : Game.t;
  mutable location : int * int;
  mutable room : Dungeon.t;
  mutable room_exit : int * int;
  mutable in_fight : bool;
  mutable fight : fight;
  mutable health : int;
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
  }

let reset_fight c =
  let new_m = Dungeon.get_monster c.room in
  c.in_fight <- false;
  c.fight.spiraled <- false;
  c.fight.action <- Attack;
  c.fight.attacking <- false;
  c.fight.monster <- new_m;
  c.fight.monster_string <- Dungeon.get_monster_string new_m;
  c.fight.monster_health <- Dungeon.get_monster_HP new_m

let fight_decision bound = Random.int bound = 0

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let map_move current key =
  let current_bound = Dungeon.get_bound current.room in
  let x, y = current.location in
  current.in_fight <-
    ( if
      current.location = current.room_exit
      || current.location = Dungeon.get_start current.room
    then false
    else fight_decision current_bound );
  if current.in_fight then
    Render_stack.stack_push Render_stack.SpiralRender;
  (* delete light right below when spiral works. it is a work around*)
  begin
    match key with
    | Glut.KEY_RIGHT ->
        current.location <-
          ( if Dungeon.is_wall current.room (x + 1, y) then (x, y)
          else (x + 1, y) )
    | Glut.KEY_LEFT ->
        current.location <-
          ( if Dungeon.is_wall current.room (x - 1, y) then (x, y)
          else (x - 1, y) )
    | Glut.KEY_UP ->
        current.location <-
          ( if Dungeon.is_wall current.room (x, y + 1) then (x, y)
          else (x, y + 1) )
    | Glut.KEY_DOWN ->
        current.location <-
          ( if Dungeon.is_wall current.room (x, y - 1) then (x, y)
          else (x, y - 1) )
    | _ -> ()
  end;
  if current.in_fight = true then Timer.reset_timer ();
  let should_change_next = current.room_exit = current.location in
  let should_change_prev =
    current.location = Dungeon.get_start current.room
  in
  if should_change_next then (
    current.room <- Game.next_dungeon current.game current.room;
    current.location <- Dungeon.get_start current.room;
    current.room_exit <- Dungeon.get_exit current.room )
  else if should_change_prev then (
    current.room <- Game.prev_dungeon current.game current.room;
    current.location <- Dungeon.get_exit current.room;
    current.room_exit <- Dungeon.get_exit current.room );
  current

let typing_case current key =
  let str = current.fight.input_string
  and mon_str = current.fight.monster_string
  and mon_HP = current.fight.monster_health in
  if key = 13 then (
    let diff = Levenshtein.dist str mon_str in
    ( match current.fight.action with
    | Attack ->
        let damage = max (String.length mon_str - diff) 0 in
        current.fight.monster_health <- max (mon_HP - damage) 0;
        if current.fight.monster_health > 0 then (
          Render_stack.stack_push Render_stack.ScreenshakeRender;
          current.fight.player_health <-
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)) );
        if damage > 0 then
          Render_stack.stack_push Render_stack.AttackRender
        else ()
    | Recover ->
        if current.fight.monster_health > 0 then (
          Render_stack.stack_push Render_stack.ScreenshakeRender;
          current.fight.player_health <-
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)) );
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
            max 0 (current.fight.player_health - max 1 (mon_HP / 20)) );
        if diff <= String.length str / 3 then (
          Render_stack.stack_pop ();
          reset_fight current ) );

    current.fight.attacking <- false;
    "" )
  else if key = 127 then
    String.sub str 0 (max (String.length str - 1) 0)
  else str ^ Char.escaped (Char.chr key)

let clamp_str str = String.sub str 0 (min (String.length str) 20)

let typing_move current key =
  if current.in_fight && current.fight.attacking then
    current.fight.input_string <- clamp_str (typing_case current key);
  current

let menu_move current key =
  ( match key with
  | Glut.KEY_RIGHT ->
      current.fight.action <- get_next_action current.fight.action
  | Glut.KEY_LEFT ->
      current.fight.action <- get_prev_action current.fight.action
  | Glut.KEY_DOWN ->
      Render_stack.stack_pop ();
      reset_fight current
  | Glut.KEY_UP ->
      Timer.reset_timer ();
      current.fight.attacking <- not current.fight.attacking
  | _ -> () );
  current

(* [controller current key] updates the [current] based on [key]*)
let controller current key =
  (* move after end of fight*)
  if current.in_fight && current.fight.monster_health = 0 then (
    Render_stack.stack_pop ();
    reset_fight current;
    current (* can't move during spiral animation*) )
  else if current.in_fight && not current.fight.spiraled then current
    (* menu move*)
  else if current.in_fight && not current.fight.attacking then
    menu_move current key (* move around map*)
  else if not current.in_fight then map_move current key
  else current

let check_time_limit current =
  if
    current.fight.attacking
    && Timer.current_time () > current.fight.typing_limit
  then typing_move current 13
  else current
