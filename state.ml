type current = {
  location : int * int;
  room : Dungeon.t;
}

let init_state dungeon =
  let current_loc = dungeon |> Dungeon.get_start in
  { location = current_loc; room = dungeon }

let player_loc state = state.location

let move coor dir =
  match dir with
  | "w" -> (fst coor, snd coor + 1)
  | "a" -> (fst coor - 1, snd coor)
  | "s" -> (fst coor, snd coor - 1)
  | "d" -> (fst coor + 1, snd coor)
  | _ -> coor
