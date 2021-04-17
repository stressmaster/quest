open Random

type action =
  | Run
  | Recover
  | Attack

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
  mutable action : action;
}

let curr_room c = c.room

let in_fight c = c.in_fight

let curr_action c = c.action

let init_state file_name =
  let g = Yojson.Basic.from_file file_name |> Game.from_json in
  let r = g |> Game.start_room in
  {
    game = g;
    room = r;
    room_exit = r |> Dungeon.get_exit;
    location = r |> Dungeon.get_start;
    in_fight = false;
    action = Run;
  }

let fight_decision bound = Random.int bound = 0

let debug_encounters c =
  if c.in_fight then print_string "Encounter!" else ()

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let map_move_controller current key =
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
  debug_encounters current;
  current

let menu_move current key =
  ( match key with
  | Glut.KEY_RIGHT -> current.action <- get_next_action current.action
  | Glut.KEY_LEFT -> current.action <- get_prev_action current.action
  | Glut.KEY_F2 -> current.in_fight <- false
  | Glut.KEY_F1 ->
      print_string
        ( match current.action with
        | Run -> " Run| "
        | Attack -> " Attack| "
        | Recover -> " Recover |" )
  | _ -> () );
  current

(* [controller current key] updates the [current] based on [key]*)
let controller current key =
  if current.in_fight then menu_move current key
  else map_move_controller current key
