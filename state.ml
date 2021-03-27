type current = {location: int*int; room= Dungeon.dungeon; visible= int*int list}

let init_state dungeon= 
  let current_loc = dungeon.start_room in
  let sight= []
  {location= dungeon.start_room; room= dungeon; }

let player_loc state= state.location

let move state dir= 
  curr= player_loc state
  match dir with
  | "w" -> {(fst curr, snd curr + 1)}
  | "a" -> {(fst curr - 1, snd curr)}
  | "s" -> {(fst curr, snd curr - 1)}
  | "d" -> {(fst curr + 1, snd curr)}
  | _ -> state