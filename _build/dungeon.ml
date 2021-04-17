type tile = {
  material : string;
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
  id : int;
  cells : (int * int, cell) Hashtbl.t;
  start : int * int;
  exit : int * int;
  dimensions : int * int;
  bound : int;
  monsters : monster list;
  next : int;
  prev : int;
}

let instantiate_monster
    name
    sprite
    hitpoints
    encounter_chance
    attack_strings =
  { name; sprite; hitpoints; encounter_chance; attack_strings }

let is_wall dungeon (x, y) =
  (Hashtbl.find dungeon.cells (x, y)).tile.is_wall

let get_start dungeon = dungeon.start

let get_monster d = List.hd d.monsters

let get_prev dungeon = dungeon.prev

let get_next dungeon = dungeon.next

let get_exit dungeon = dungeon.exit

let get_dimensions dungeon = dungeon.dimensions

let get_cells dungeon = dungeon.cells

let get_tile cell = cell.tile

let tile_material tile = tile.material

let get_bound dungeon = dungeon.bound

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
        then { material = "wall.jpg"; is_wall = true }
        else { material = "path.jpg"; is_wall = false }
      in
      Hashtbl.add dungeon_cells (counter_x, counter_y)
        { tile; x = counter_x; y = counter_y }
    done
  done

(* [instantiate_dungeon x y] is a dugeon with [x] columns [y] rows *)
let instantiate_dungeon id x y start exit bound monsters next prev : t =
  let c = Hashtbl.create (x * y) in
  instantiate_dungeon_cells x y c;
  {
    id;
    cells = c;
    start;
    exit;
    dimensions = (x, y);
    bound;
    monsters;
    next;
    prev;
  }

let get_id d = d.id

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

let determine_color tile =
  let material = tile |> tile_material in
  if material = "wall.jpg" then Magic_numbers.wall
  else if material = "path.jpg" then Magic_numbers.path
  else Magic_numbers.darkness

(* [get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length] is the
   bounds for rendering based on [(p_x, p_y) dungeon_x_length
   dungeon_y_length] *)
let get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length =
  let x_start =
    if p_x - ((Magic_numbers.x_length - 1) / 2) < 0 then 0
    else if
      p_x + ((Magic_numbers.x_length - 1) / 2) + 1 > dungeon_x_length
    then dungeon_x_length - Magic_numbers.x_length
    else p_x - ((Magic_numbers.x_length - 1) / 2)
  in
  let x_end =
    if p_x + ((Magic_numbers.x_length - 1) / 2) + 1 > dungeon_x_length
    then dungeon_x_length
    else if p_x - ((Magic_numbers.x_length - 1) / 2) < 0 then
      Magic_numbers.x_length
    else p_x + ((Magic_numbers.x_length - 1) / 2)
  in
  let y_start =
    if p_y - ((Magic_numbers.y_length - 1) / 2) < 0 then 0
    else if
      p_y + ((Magic_numbers.y_length - 1) / 2) + 1 > dungeon_y_length
    then dungeon_y_length - Magic_numbers.y_length
    else p_y - ((Magic_numbers.y_length - 1) / 2)
  in
  let y_end =
    if p_y + ((Magic_numbers.y_length - 1) / 2) + 1 > dungeon_y_length
    then dungeon_y_length
    else if p_y - ((Magic_numbers.y_length - 1) / 2) < 0 then
      Magic_numbers.y_length
    else p_y + ((Magic_numbers.y_length - 1) / 2)
  in
  (x_start, x_end, y_start, y_end)

(* [determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon] is the
   texture of the tile at [(x, y)] based on [(p_x, p_y) dungeon_cells
   dungeon] *)
let determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon =
  if (x, y) = (p_x, p_y) then Magic_numbers.player
  else if Hashtbl.find_opt dungeon_cells (x, y) = None then
    Magic_numbers.darkness
  else if (x, y) = get_start dungeon then Magic_numbers.entrance
  else if get_exit dungeon = (x, y) then Magic_numbers.exit_tex
  else determine_color (Hashtbl.find dungeon_cells (x, y) |> get_tile)

(* [render_dungeon (p_x, p_y) (dungeon : Dungeon.t)] renders [dungeon]
   based on [(p_x, p_y)]*)
let render_dungeon (p_x, p_y) (dungeon : t) =
  let dungeon_cells = dungeon |> get_cells in
  let dungeon_x_length, dungeon_y_length = dungeon |> get_dimensions in
  let x_start, x_end, y_start, y_end =
    get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length
  in
  for x = x_start to x_end do
    for y = y_start to y_end do
      let new_texture =
        determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon
      in
      Render.render_square
        (Render.new_square
           ( float_of_int (x - x_start)
           /. float_of_int Magic_numbers.x_length
           *. 2. )
           ( float_of_int (y - y_start)
           /. float_of_int Magic_numbers.y_length
           *. 2. )
           Magic_numbers.width Magic_numbers.height new_texture)
    done
  done
