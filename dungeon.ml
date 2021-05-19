type tile = {
  material : string;
  is_wall : bool;
  item : Item.t option;
  npc : Npc.t option;
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
    | [] -> failwith "invalid index"
    | h :: t -> if cur == num then h else listsearcher t (cur + 1) num
  in
  listsearcher ourstringlist 0 ourint

let get_monster_string m =
  (* print_int (List.length m.attack_strings); *)
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

let get_item dungeon (x, y) =
  let cell = Hashtbl.find dungeon.cells (x, y) in
  let tile = cell.tile in
  let path_tile = { tile with material = "path.jpg"; item = None } in
  Hashtbl.add dungeon.cells (x, y) { cell with tile = path_tile };
  tile.item

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

let get_monsters dungeon = dungeon.monsters

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
        then
          {
            material = "wall.jpg";
            is_wall = true;
            item = None;
            npc = None;
          }
        else
          {
            material = "path.jpg";
            is_wall = false;
            item = None;
            npc = None;
          }
      in
      Hashtbl.add dungeon_cells (counter_x, counter_y)
        { tile; x = counter_x; y = counter_y }
    done
  done

let noexn_hashtable_find table elt =
  match try Some (Hashtbl.find table elt) with Not_found -> None with
  | Some a -> true
  | None -> false

let become_npc_helper x y table =
  match
    try Some (Hashtbl.find table (x, y)) with Not_found -> None
  with
  | Some cell ->
      let { tile; x; y } = cell in
      let { material; is_wall; item; npc } = tile in
      if material = "path.jpg" then 1 else 0
  | None -> 0

let tile_is_path x y table =
  match
    try Some (Hashtbl.find table (x, y)) with Not_found -> None
  with
  | Some { tile; x; y } ->
      let { material; is_wall; item; npc } = tile in
      not is_wall
  | None -> false

let become_npc x y table =
  let visibility =
    if tile_is_path x y table then 0
    else
      (become_npc_helper (x + 1) y table
      + become_npc_helper (x - 1) y table
      + become_npc_helper x (y - 1) table
      + become_npc_helper x (y + 1) table)
      * Random.int 12
  in
  if visibility >= 22 then true else false

let render_npc_speech x y dungeon_cells =
  match
    try Some (Hashtbl.find dungeon_cells (x, y))
    with Not_found -> None
  with
  | None -> ()
  | Some { tile; x; y } -> (
      let { material; is_wall; item; npc } = tile in
      match npc with
      | Some n ->
          let speech = Npc.get_npc_speech n in
          Font.render_font
            (Font.new_font speech 0. 1. Magic_numbers.width
               Magic_numbers.height)
      | None -> ())

let add_npc_shadow x y dungeon_cells npc_shadow =
  match
    try Some (Hashtbl.find dungeon_cells (x, y))
    with Not_found -> None
  with
  | None -> ()
  | Some { tile; x; y } ->
      let { material; is_wall; item; npc } = tile in
      if not is_wall then
        let tile = { tile with npc = npc_shadow } in
        Hashtbl.add dungeon_cells (x, y) { tile; x; y }

let add_npc_shadows x y dungeon_cells npc_shadow =
  add_npc_shadow (x - 1) y dungeon_cells npc_shadow;
  add_npc_shadow (x + 1) y dungeon_cells npc_shadow;
  add_npc_shadow x (y + 1) dungeon_cells npc_shadow;
  add_npc_shadow x (y - 1) dungeon_cells npc_shadow

let add_npcs x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if become_npc counter_x counter_y dungeon_cells then (
        let npc = Npc.get_npc (Random.int 4) in
        let tile =
          {
            material = Npc.get_npc_sprite npc;
            is_wall = true;
            item = None;
            npc = Some npc;
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y };
        add_npc_shadows counter_x counter_y dungeon_cells (Some npc))
      else ()
    done
  done

let add_items x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if
        tile_is_path counter_x counter_y dungeon_cells
        && Random.int 20 > 18
      then
        let item_type = Random.bool () in
        let item = Item.create_item 1 item_type in
        let tile =
          {
            material = Item.get_item_sprite item;
            is_wall = false;
            item = Some item;
            npc = None;
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done

let instantiate_dungeon_cells2 x y dungeon_cells lst =
  let rec listsearcher dungeon_cells lst =
    match lst with
    | [] -> ()
    | h :: t ->
        let tile =
          {
            material = "path.jpg";
            is_wall = false;
            item = None;
            npc = None;
          }
        in
        Hashtbl.add dungeon_cells h { tile; x = fst h; y = snd h };
        listsearcher dungeon_cells t
  in
  listsearcher dungeon_cells lst;
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if
        noexn_hashtable_find dungeon_cells (counter_x, counter_y)
        = false
      then
        let tile =
          {
            material = "wall.jpg";
            is_wall = true;
            item = None;
            npc = None;
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done;
  add_npcs x y dungeon_cells;
  add_items x y dungeon_cells

type direction =
  | Up
  | Down
  | Right
  | Left

let rec list_element lst k =
  match lst with
  | [] -> failwith "invalid index"
  | h :: t -> if k = 0 then h else list_element t (k - 1)

let randdir dir =
  let ourrand = Random.int 2 in
  match dir with
  | Up ->
      let newlst = [ Right; Left ] in
      list_element newlst ourrand
  | Right ->
      let newlst = [ Up; Down ] in
      list_element newlst ourrand
  | Down ->
      let newlst = [ Right; Left ] in
      list_element newlst ourrand
  | Left ->
      let newlst = [ Up; Down ] in
      list_element newlst ourrand

let rec carver cur_x cur_y x_bound y_bound dir lst more bigmore =
  let newlst = (cur_x, cur_y) :: lst in
  if bigmore = 0 then lst
  else
    match dir with
    | Right ->
        if cur_x + 1 >= x_bound || more = 0 then
          carver cur_x cur_y x_bound y_bound (randdir dir) newlst
            (Random.int x_bound) bigmore
        else
          carver (cur_x + 1) cur_y x_bound y_bound Right newlst
            (more - 1) (bigmore - 1)
    | Left ->
        if cur_x - 1 <= 0 || more = 0 then
          carver cur_x cur_y x_bound y_bound (randdir dir) newlst
            (Random.int x_bound) bigmore
        else
          carver (cur_x - 1) cur_y x_bound y_bound Left newlst
            (more - 1) (bigmore - 1)
    | Up ->
        if cur_y + 1 >= y_bound || more = 0 then
          carver cur_x cur_y x_bound y_bound (randdir dir) newlst
            (Random.int y_bound) bigmore
        else
          carver cur_x (cur_y + 1) x_bound y_bound Up newlst (more - 1)
            (bigmore - 1)
    | Down ->
        if cur_y - 1 <= 0 || more = 0 then
          carver cur_x cur_y x_bound y_bound (randdir dir) newlst
            (Random.int x_bound) bigmore
        else
          carver cur_x (cur_y - 1) x_bound y_bound Down newlst
            (more - 1) (bigmore - 1)

(* [instantiate_dungeon x y] is a dungeon with [x] columns [y] rows *)
let instantiate_dungeon id x y start exit bound monsters next prev : t =
  Random.init id;
  let c = Hashtbl.create (x * y) in
  (* let ourlst = carver 0 0 x y Right [] 5 300 in *)
  let w = Walker.init_walker 0 x 0 y in
  let ourlst = Walker.walk 2600 w in
  instantiate_dungeon_cells2 x y c ourlst;
  (* instantiate_dungeon_cells x y c;*)
  {
    id;
    cells = c;
    start;
    exit = w.furthest_pos;
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
  match tile.item with
  | Some (Armor _) -> Magic_numbers.armor_pickup_png
  | Some (Weapon _) -> Magic_numbers.weapon_pickup_png
  | _ ->
      let material = tile |> tile_material in
      if material = "wall.jpg" then Magic_numbers.wall
      else if material = "path.jpg" then Magic_numbers.path
      else if material = "monster.png" then Magic_numbers.monster
      else if material = "goblin_1.jpg" then Magic_numbers.goblin_1
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
  if (x, y) = (p_x, p_y) then Spriteanimation.get_sprite "player"
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
let render_dungeon (p_x, p_y) (dungeon : t) condition =
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
    (float_of_int dungeon_x_length, float_of_int dungeon_y_length);
  (* if Render_stack.stack_peek () != Render_stack.SpiralRender then *)
  if condition then render_npc_speech p_x p_y dungeon_cells

(* Render_stack.stack_push Render_stack.DungeonRender *)
