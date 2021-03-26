type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}
type dungeon = tile list list 
let create_4x4_room = 
  let wall = {sprite = "wall.jpg"; is_wall = true;} in
  let path = {sprite = "path.jpg"; is_wall = false;} in 
  let layer1 = [wall; wall; wall; wall] in 
  let layer2 = [wall; path; path; wall] in
  [layer1; layer2; layer2; layer1]