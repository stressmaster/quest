type tile_sprite = string
type color = | Green | Gray
type material = 
| Color of color
| Sprite of tile_sprite

type tile = {material : material; is_wall : bool}

type cell = {tile: tile; x : int; y : int}

type dungeon = {
  cells : (int * int, cell) Hashtbl.t;
  start : (int * int); 
  dimensions : (int * int);
}

(* [instantiate_dungeon_cells x y dungeon_cells] associates (x', y') with a tile that has x-cord x' and y-cord y' for 0<=x'<=[x]-1 and 0<=y'<=[y]-1 in dungeon_cells *)
let instantiate_dungeon_cells x y dungeon_cells= 
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if counter_y = 0 || counter_y = y || counter_x =0 || counter_x = x then
        Hashtbl.add dungeon_cells (counter_x,counter_y) {tile = {material = Sprite "wall.jpg"; is_wall = true}; x =counter_x; y=counter_y}
      else
        Hashtbl.add dungeon_cells (counter_x,counter_y) {tile = {material = Sprite "path.jpg"; is_wall = false}; x=counter_x; y=counter_y}
      done
    done

(* [instantiate_dungeon x y] is a dugeon with [x] columns [y] rows *)
let instantiate_dungeon x y : dungeon=
  let c = Hashtbl.create (x*y) in
  instantiate_dungeon_cells x y c;
  {cells = c; start = (1,1); dimensions = (x,y)}

let print_dungeon dungeon = 
  for y = 0 to dungeon.dimensions |> fst do
    if y > 0 then
    print_newline()
    else ();
    for x = 0 to dungeon.dimensions |> snd do
      if ((Hashtbl.find dungeon.cells (x, y)).tile.is_wall)
        then print_string("#")
      else
        print_string(".")
    done
  done

