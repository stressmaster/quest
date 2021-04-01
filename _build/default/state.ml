type current = {
  mutable location : int * int;
  mutable room : Dungeon.t;
}

let init_state dungeon =
  { location = dungeon |> Dungeon.get_start; room = dungeon }

let player_loc state = state.location

(* [move current key] assigns a location to [current] based on [key]*)
let move current key =
  let x, y = current.location in
  match key with
  | Glut.KEY_RIGHT ->
      current.location <-
        ( if Dungeon.is_wall current.room (x + 1, y) then (x, y)
        else (x + 1, y) );
      current
  | Glut.KEY_LEFT ->
      current.location <-
        ( if Dungeon.is_wall current.room (x - 1, y) then (x, y)
        else (x - 1, y) );
      current
  | Glut.KEY_UP ->
      current.location <-
        ( if Dungeon.is_wall current.room (x, y + 1) then (x, y)
        else (x, y + 1) );
      current
  | Glut.KEY_DOWN ->
      current.location <-
        ( if Dungeon.is_wall current.room (x, y - 1) then (x, y)
        else (x, y - 1) );
      current
  | _ -> current
