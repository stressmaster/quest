open Random

type current = {
  mutable location : int * int;
  mutable room : Dungeon.t;
  mutable in_fight : bool;
}

let init_state dungeon =
  {
    location = dungeon |> Dungeon.get_start;
    room = dungeon;
    in_fight = false;
  }

let fight_decision bound =
  match Random.int bound with 0 -> true | _ -> false

let debug_encounters c =
  if c.in_fight then print_string "Encounter!" else ()

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let move current key =
  let current_bound = Dungeon.get_bound current.room in
  let x, y = current.location in
  match key with
  | Glut.KEY_RIGHT ->
      current.location <-
        (if Dungeon.is_wall current.room (x + 1, y) then (x, y)
        else (x + 1, y));
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current;
      (*debug stuff end*)
      current
  | Glut.KEY_LEFT ->
      current.location <-
        (if Dungeon.is_wall current.room (x - 1, y) then (x, y)
        else (x - 1, y));
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current;
      (*debug stuff end*)
      current
  | Glut.KEY_UP ->
      current.location <-
        (if Dungeon.is_wall current.room (x, y + 1) then (x, y)
        else (x, y + 1));
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current;
      (*debug stuff end*)
      current
  | Glut.KEY_DOWN ->
      current.location <-
        (if Dungeon.is_wall current.room (x, y - 1) then (x, y)
        else (x, y - 1));
      current.in_fight <- fight_decision current_bound;
      (*debug stuff start*)
      debug_encounters current;
      (*debug stuff end*)
      current
  | _ -> current
