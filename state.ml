type current = {
  mutable location : int * int;
  mutable room : Dungeon.t;
}

let init_state dungeon =
  { location = dungeon |> Dungeon.get_start; room = dungeon }

let player_loc state = state.location

let move current key =
  let x, y = current.location in
  match key with
  | Glut.KEY_RIGHT ->
      current.location <- (x + 1, y);
      current
  | Glut.KEY_LEFT ->
      current.location <- (x - 1, y);
      current
  | Glut.KEY_UP ->
      current.location <- (x, y + 1);
      current
  | Glut.KEY_DOWN ->
      current.location <- (x, y - 1);
      current
  | _ -> current
