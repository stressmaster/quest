type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}
type dungeon = tile list list 
let create_4x4_room = 
  let wall = {sprite = "wall.jpg"; is_wall = true;} in
  let path = {sprite = "path.jpg"; is_wall = false;} in 
  let layer1 = [wall; wall; wall; wall] in 
  let layer2 = [wall; path; path; wall] in
  [layer1; layer2; layer2; layer1]


let rec print_dungeon_row acc row =
  match row with
| [] -> acc
| {sprite; is_wall} :: t -> if is_wall then
  print_dungeon_row (acc ^ "#") t else
    print_dungeon_row (acc ^ ".") t

let rec print_dungeon dungeon = 
match dungeon with
| [] -> ()
| x :: x' -> print_string (print_dungeon_row "" x); print_newline(); print_dungeon x'

