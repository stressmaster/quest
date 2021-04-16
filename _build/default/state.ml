open Random

type action =
  | Run
  | Recover
  | Attack

let get_next_action = function
  | Run -> Attack
  | Recover -> Run
  | Attack -> Recover

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
}

let curr_room c = c.room

let init_state file_name =
  let g = Yojson.Basic.from_file file_name |> Game.from_json in
  let r = g |> Game.start_room in
  {
    game = g;
    room = r;
    room_exit = r |> Dungeon.get_exit;
    location = r |> Dungeon.get_start;
    in_fight = false;
  }

let fight_decision bound = Random.int bound = 0

let debug_encounters c =
  if c.in_fight then print_string "Encounter!" else ()

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let map_move_controller current key =
  let current_bound = Dungeon.get_bound current.room in
  let x, y = current.location in
  ( match key with
  | Glut.KEY_RIGHT ->
      current.location <-
        ( if Dungeon.is_wall current.room (x + 1, y) then (x, y)
        else (x + 1, y) );
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current (*debug stuff end*)
  | Glut.KEY_LEFT ->
      current.location <-
        ( if Dungeon.is_wall current.room (x - 1, y) then (x, y)
        else (x - 1, y) );
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current (*debug stuff end*)
  | Glut.KEY_UP ->
      current.location <-
        ( if Dungeon.is_wall current.room (x, y + 1) then (x, y)
        else (x, y + 1) );
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current (*debug stuff end*)
  | Glut.KEY_DOWN ->
      current.location <-
        ( if Dungeon.is_wall current.room (x, y - 1) then (x, y)
        else (x, y - 1) );
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current (*debug stuff end*)
  | _ -> () );

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
  current
