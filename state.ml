type current = {location: int*int; room= Dungeon.dungeon; visible= int*int list}

let init_state dungeon= 
  let current_loc = dungeon.start_room in
  let x= fst current_loc in
  let y = snd current_loc in
  let sight= [(x-2, y+2); (x-1, y+2); (x, y+2); (x+1, y+2); (x+2, y+2);
  (x-2, y+1); (x-1, y+1); (x, y+1); (x+1, y+1); (x+2, y+1); (x-2, y);
  (x-1, y); (x, y); (x+1, y); (x+2, y); (x-2, y-1); (x-1, y-1); (x, y-1);
  (x+1, y-1); (x+2, y-1); (x-2, y-2); (x-1, y-2); (x, y-2); (x+1, y-2); (x+2, y-2)]
  {location= dungeon.start_room; room= dungeon; visible=sight}

let player_loc state= state.location

let move coor dir= 
  match dir with
  | "w" -> {(fst coor, snd coor + 1)}
  | "a" -> {(fst coor - 1, snd coor)}
  | "s" -> {(fst coor, snd coor - 1)}
  | "d" -> {(fst coor + 1, snd coor)}
  | _ -> coor