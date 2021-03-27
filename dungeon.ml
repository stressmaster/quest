type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}

type cell = {tile: tile; x : int; y : int}

type dungeon = {
  cells : (int * int, cell) Hashtbl.t;
  start : (int * int); 
}
let spawn_dungeon x y dungeon_cells= 

  for y = 0 to counter_y do
    for x = 0 to counter_x do
      if y = 0 || y = counter_y or x =0 or x = counter_x then
        Hashtbl.add dungeon_cells (counter_x,counter_y) {tile = {sprite = "wall.jpg"; is_wall = true}; x=counter_x; y=counter_y};
      else
        Hashtbl.add dungeon_cells (counter_x,counter_y) {tile = {sprite = "path.jpg"; is_wall = false}; x=counter_x; y=counter_y};
      done
    done

let dungeon_4x4 = {cells = spawn_dungeon 4 4 (Hashtbl.create (16)); start = (0,0)}

let print_dungeon counter_x counter_y dungeon= 
  for y = 0 to counter_y do
    for x = 0 to counter_x do
      let c = if (Hashtbl.find dungeon.cells (x, y).tile.is_wall)
        then "#"
      else
        "."
    in
    print_string(c);
  done
      print_newline();
done
