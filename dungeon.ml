type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}

type cell = {tile: tile; x : int; y : int}

type dungeon = {
  cells : (int * int, cell) Hashtbl.t;
  start : (int * int);
end 
}
let spawn_dungeon x y dungeon_cells= 
  
  let rec helper counter_x counter_y=
    if counter_x = x && counter_y = y then
      ();
    if counter_x <> x then
    Hashtbl.add dungeon_cells (counter_x,counter_y) {tile = {sprite = "wall.jpg"; is_wall = false}; x=counter_x; y=counter_y};
    helper (counter_x + 1) counter_y;
    if counter_x = x && counter_y <> y then  
      Hashtbl.add dungeon_cells (counter_x, counter_y) {tile = {sprite = "path.jpg"; is_wall = false}; x=counter_x; y=counter_y};
      helper 0 (counter_y + 1);
    in
  helper 0 0

let dungeon_4x4 = {cells = spawn_dungeon 4 4 (Hashtbl.create (16)); start = (0,0)}

let rec print_dungeon_row acc row =
  match row with
| [] -> acc
| {sprite; is_wall} :: t -> if is_wall then
  print_dungeon_row (acc ^ "#") t else
    print_dungeon_row (acc ^ ".") t

let rec print_dungeon dungeon = 
match dungeon.tiles with
| [] -> ()
| x :: x' -> print_string (print_dungeon_row "" x); print_newline(); print_dungeon x'

