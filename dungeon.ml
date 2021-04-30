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
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
}

let monster_move (mon : monster) =
  let ourstringlist = mon.attack_strings in
  let ourlen = List.length ourstringlist in
  let ourint = Random.int ourlen in
  let rec listsearcher lst cur num =
    match lst with
    | [] -> failwith "wtf"
    | h :: t -> if cur == num then h else listsearcher t (cur + 1) num
  in
  listsearcher ourstringlist 0 ourint

let get_monster_string m =
  print_int (List.length m.attack_strings);
  let no = Random.int (List.length m.attack_strings) in
  List.nth m.attack_strings no

let get_monster_HP m = m.hitpoints

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

let get_monster d =
  let total =
    List.fold_left (fun acc m -> acc + m.encounter_chance) 0 d.monsters
  in
  let no = Random.int total + 1 in
  let rec find_m acc = function
    | h :: t ->
        if h.encounter_chance + acc >= no then h
        else find_m (h.encounter_chance + acc) t
    | [] -> List.hd d.monsters
  in
  find_m 0 d.monsters

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

let noexn_hashtable_find table elt =
  match try Some (Hashtbl.find table elt) with Not_found -> None with
  | Some a -> true
  | None -> false

let instantiate_dungeon_cells2 x y dungeon_cells lst =
  let rec listsearcher dungeon_cells lst =
    match lst with
    | [] -> ()
    | h :: t ->
        let tile = { material = "path.jpg"; is_wall = false } in
        Hashtbl.add dungeon_cells h { tile; x = fst h; y = snd h };
        listsearcher dungeon_cells t
  in
  listsearcher dungeon_cells lst;
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      let tile =
        if
          noexn_hashtable_find dungeon_cells (counter_x, counter_y)
          = false
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

let render_mini_map (p_x, p_y) (dungeon_x_length, dungeon_y_length) =
  let starting_y =
    2.
    *. (1. -. (dungeon_y_length *. Magic_numbers.height /. 10. /. 500.))
  in
  Render.render_square
    (Render.new_square 0. starting_y
       (dungeon_x_length *. Magic_numbers.width /. 10.)
       (dungeon_y_length *. Magic_numbers.height /. 10.)
       Magic_numbers.darkness);

  Render.render_square
    (Render.new_square
       (p_x *. 2.
       /. (dungeon_x_length +. 2.)
       *. (dungeon_x_length *. Magic_numbers.width /. 10.)
       /. 500.)
       (starting_y
       +. p_y
          /. (dungeon_y_length +. 2.)
          *. 2.
          *. (dungeon_y_length *. Magic_numbers.height /. 10.)
          /. 500.)
       10. 10. "./fonts/i.png")

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
           (float_of_int (x - x_start)
           /. float_of_int Magic_numbers.x_length
           *. 2.)
           (float_of_int (y - y_start)
           /. float_of_int Magic_numbers.y_length
           *. 2.)
           Magic_numbers.width Magic_numbers.height new_texture)
    done
  done;
  render_mini_map
    (float_of_int p_x, float_of_int p_y)
    (float_of_int dungeon_x_length, float_of_int dungeon_y_length)

(* Render_stack.stack_push Render_stack.DungeonRender *)
