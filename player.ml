type player = {location: int*int}

let init_player tup= {tup}

let player_loc player= player.location

let move player dir= 
  curr= player_loc player
  match dir with
  | "w" -> {(fst curr, snd curr + 1)}
  | "a" -> {(fst curr - 1, snd curr)}
  | "s" -> {(fst curr, snd curr - 1)}
  | "d" -> {(fst curr + 1, snd curr)}
  | _ -> player