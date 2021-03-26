type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}
type dungeon = {tiles : tile list list; start : tile; exit : tile} 
let create_4x4_room = 
  let wall = {sprite = "wall.jpg"; is_wall = true;} in
  let path = {sprite = "path.jpg"; is_wall = false;} in 
  let start = {sprite = "start.jpg"; is_wall = false;} in 
  let exit = {sprite = "exit.jpg"; is_wall = false;} in 
  let layer1 = [wall; wall; wall; wall] in 
  let layer2 = [wall; start; path; wall] in
  let layer3 = [wall; path; path; wall] in
  let layer4 = [wall; path; exit; wall] in
  { tiles = [layer1; layer2; layer3; layer4]; start = start; exit = exit}


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

