open Random

type action =
  | Run
  | Recover
  | Attack

type fight = {
  mutable action : action;
  mutable attacking : bool;
  mutable monster : Dungeon.monster;
  mutable monster_string : string;
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
        action = Attack;
        attacking = false;
        monster = m;
        monster_string = Dungeon.get_monster_string m;
        input_string = "";
      };
    health = Magic_numbers.health;
  }

let fight_decision bound = Random.int bound = 0

let debug_encounters c =
  if c.in_fight then print_string "Encounter!" else ()

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let map_move current key =
  let current_bound = Dungeon.get_bound current.room in
  let x, y = current.location in
  current.in_fight <- fight_decision current_bound;
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
  let should_change_room = current.room_exit = current.location in
  current.room <-
    ( if should_change_room then
      Game.next_dungeon current.game current.room
    else current.room );
  current.location <-
    ( if should_change_room then Dungeon.get_start current.room
    else current.location );
  current.room_exit <-
    ( if should_change_room then Dungeon.get_exit current.room
    else current.room_exit );
  debug_encounters current;
  current

let typing_move current key =
  if current.in_fight && current.fight.attacking then
    current.fight.input_string <-
      current.fight.input_string ^ Char.escaped (Char.chr key);
  current

let menu_move current key =
  ( match key with
  | Glut.KEY_RIGHT ->
      current.fight.action <- get_next_action current.fight.action
  | Glut.KEY_LEFT ->
      current.fight.action <- get_prev_action current.fight.action
  | Glut.KEY_F2 -> current.in_fight <- false
  | Glut.KEY_F1 ->
      current.fight.attacking <- not current.fight.attacking
  | _ -> () );
  current

(* [controller current key] updates the [current] based on [key]*)
let controller current key =
  if current.in_fight && not current.fight.attacking then
    menu_move current key
  else if not current.in_fight then map_move current key
  else current
