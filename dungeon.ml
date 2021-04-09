type tile_sprite = string

type material = Sprite of tile_sprite

type tile = {
  material : material;
  is_wall : bool;
}

type cell = {
  tile : tile;
  x : int;
  y : int;
}

type monster = {
  name : string;
  sprite : string;
  hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
}

type t = {
  cells : (int * int, cell) Hashtbl.t;
  start : int * int;
  exit : int * int;
  dimensions : int * int;
  difficulty : int;
  monsters : monster list;
  next : t option;
  prev : t option;
}

(* [instantiate_dungeon_cells x y dungeon_cells] associates (x', y')
   with a tile that has x-cord x' and y-cord y' for 0<=x'<=[x]-1 and
   0<=y'<=[y]-1 in dungeon_cells *)
let instantiate_dungeon_cells x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      let tile =
        if
          counter_y = 0
          || counter_y = y - 1
          || counter_x = 0
          || counter_x = x - 1
        then { material = Sprite "wall.jpg"; is_wall = true }
        else { material = Sprite "path.jpg"; is_wall = false }
      in
      Hashtbl.add dungeon_cells (counter_x, counter_y)
        { tile; x = counter_x; y = counter_y }
    done
  done

(* [instantiate_dungeon x y] is a dugeon with [x] columns [y] rows *)
let instantiate_dungeon x y b : t =
  let c = Hashtbl.create (x * y) in
  instantiate_dungeon_cells x y c;
  {
    cells = c;
    start = (1, 1);
    exit = (x - 2, y - 2);
    dimensions = (x, y);
    bound = b;
  }

let print_dungeon dungeon =
  for y = 0 to dungeon.dimensions |> fst do
    if y > 0 then print_newline () else ();
    for x = 0 to dungeon.dimensions |> snd do
      let c = ref "." in
      if (Hashtbl.find dungeon.cells (x, y)).tile.is_wall then c := "#"
      else if (x, y) = dungeon.start then c := "<"
      else if (x, y) = dungeon.exit then c := ">";
      print_string !c
    done
  done

let is_wall dungeon (x, y) =
  (Hashtbl.find dungeon.cells (x, y)).tile.is_wall

let get_start dungeon = dungeon.start

let get_exit dungeon = dungeon.exit

let get_dimensions dungeon = dungeon.dimensions

let get_cells dungeon = dungeon.cells

let get_tile cell = cell.tile

let tile_material tile = tile.material

let get_bound dungeon = dungeon.bound
