type tile = {
  material : string;
  is_wall : bool;
  item : Item.t option;
  npc : Npc.t option * int * int;
}

type cell = {
  tile : tile;
  x : int;
  y : int;
}

type t = {
  id : int;
  cells : (int * int, cell) Hashtbl.t;
  start : int * int;
  exit : int * int;
  dimensions : int * int;
  bound : int;
  next : int;
  prev : int;
  magic_numbers : Magic_numbers.t;
  time : int;
}

let is_wall dungeon (x, y) =
  (Hashtbl.find dungeon.cells (x, y)).tile.is_wall

let get_item dungeon (x, y) =
  let cell = Hashtbl.find dungeon.cells (x, y) in
  let tile = cell.tile in
  let path_tile =
    { tile with material = !Magic_numbers.get_magic.path; item = None }
  in
  Hashtbl.add dungeon.cells (x, y) { cell with tile = path_tile };
  tile.item

let drop_item dungeon (x, y) item =
  let cell = Hashtbl.find dungeon.cells (x, y) in
  let tile = cell.tile in
  let path_tile =
    { tile with material = !Magic_numbers.get_magic.path; item }
  in
  Hashtbl.add dungeon.cells (x, y) { cell with tile = path_tile };
  ()

let get_start dungeon = dungeon.start

let get_prev dungeon = dungeon.prev

let get_next dungeon = dungeon.next

let get_exit dungeon = dungeon.exit

let get_dimensions dungeon = dungeon.dimensions

let get_time dungeon = dungeon.time

let get_cells dungeon = dungeon.cells

let get_tile cell = cell.tile

let tile_material tile = tile.material

let get_bound dungeon = dungeon.bound

let tile_maker material is_wall item npc =
  { material; is_wall; item; npc }

let noexn_hashtable_find table elt =
  match Hashtbl.find_opt table elt with Some a -> true | None -> false

let become_npc_helper x y table =
  match Hashtbl.find_opt table (x, y) with
  | Some cell ->
      let { tile; x; y } = cell in
      let { material; is_wall; item; npc } = tile in
      if material = !Magic_numbers.get_magic.path then 1 else 0
  | None -> 0

let tile_is_path x y table =
  match Hashtbl.find_opt table (x, y) with
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
      * Random.int 7
  in
  if visibility >= 22 then true else false

let render_npc_speech x y dungeon_cells =
  match Hashtbl.find_opt dungeon_cells (x, y) with
  | None -> ()
  | Some { tile; x; y } -> (
      let { material; is_wall; item; npc } = tile in
      match npc with
      | Some n, _, _ ->
          let speech = Npc.get_npc_speech n in
          Font.render_font ~spacing:0.05
            (Font.new_font speech 1.1 1.2
               (Magic_numbers.width *. 0.5)
               (Magic_numbers.height *. 0.5))
      | None, _, _ -> ())

let add_npc_shadow x y dungeon_cells npc_shadow dir_x dir_y =
  match Hashtbl.find_opt dungeon_cells (x, y) with
  | None -> ()
  | Some { tile; x; y } ->
      let { material; is_wall; item; npc } = tile in
      if not is_wall then
        let tile = { tile with npc = (npc_shadow, dir_x, dir_y) } in
        Hashtbl.add dungeon_cells (x, y) { tile; x; y }

let add_npc_shadows x y dungeon_cells npc_shadow =
  add_npc_shadow (x - 1) y dungeon_cells npc_shadow 1 0;
  add_npc_shadow (x + 1) y dungeon_cells npc_shadow (-1) 0;
  add_npc_shadow x (y + 1) dungeon_cells npc_shadow 0 (-1);
  add_npc_shadow x (y - 1) dungeon_cells npc_shadow 0 1

let add_npcs x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if become_npc counter_x counter_y dungeon_cells then (
        Random.self_init ();
        let npc =
          Npc.get_npc
            (Random.int (Npc.npc_list_length !Magic_numbers.get_magic))
            !Magic_numbers.get_magic
        in
        let tile =
          tile_maker (Npc.get_npc_sprite npc) true None (Some npc, 0, 0)
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y };
        add_npc_shadows counter_x counter_y dungeon_cells (Some npc))
      else ()
    done
  done

let should_add_item counter_x counter_y dungeon_cells =
  tile_is_path counter_x counter_y dungeon_cells && Random.int 40 = 0

let add_items x y dungeon_cells id =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if should_add_item counter_x counter_y dungeon_cells then
        let item =
          Item.create_item id (Random.bool ()) !Magic_numbers.get_magic
        in
        let npc =
          (Hashtbl.find dungeon_cells (counter_x, counter_y)).tile.npc
        in
        let tile =
          tile_maker (Item.get_item_sprite item) false (Some item) npc
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done

let instantiate_dungeon_cells_helper x y dungeon_cells =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if
        noexn_hashtable_find dungeon_cells (counter_x, counter_y)
        = false
      then
        let tile =
          tile_maker !Magic_numbers.get_magic.wall true None (None, 0, 0)
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done

let instantiate_dungeon_cells x y dungeon_cells lst id =
  let rec listsearcher dungeon_cells lst =
    match lst with
    | [] -> ()
    | h :: t ->
        let tile =
          tile_maker !Magic_numbers.get_magic.path false None
            (None, 0, 0)
        in
        Hashtbl.add dungeon_cells h { tile; x = fst h; y = snd h };
        listsearcher dungeon_cells t
  in
  listsearcher dungeon_cells lst;
  instantiate_dungeon_cells_helper x y dungeon_cells;
  if Npc.npc_list_length !Magic_numbers.get_magic > 0 then
    add_npcs x y dungeon_cells;
  add_items x y dungeon_cells id

let instantiate_dungeon_helper
    id
    cells
    start
    exit
    dimensions
    bound
    next
    prev
    magic_numbers
    time =
  {
    id;
    cells;
    start;
    exit;
    dimensions;
    bound;
    next;
    prev;
    magic_numbers;
    time;
  }

let instantiate_dungeon
    ?(seed = Timer.current_time "big")
    id
    x
    y
    start
    bound
    next
    prev : t =
  Random.init seed;
  let magic_numbers =
    Magic_numbers.init (Random.int Magic_numbers.length)
  in
  Magic_numbers.update magic_numbers;
  let c = Hashtbl.create (x * y) in
  let w = Walker.init_walker 0 x 0 y start in
  let ourlst = Walker.walk 6500 w in
  instantiate_dungeon_cells x y c ourlst id;
  instantiate_dungeon_helper id c start w.furthest_pos (x, y) bound next
    prev magic_numbers seed

let get_id d = d.id

let determine_color tile =
  match tile.item with
  | Some (Armor _) -> !Magic_numbers.get_magic.items.armor_pickup
  | Some (Weapon _) -> !Magic_numbers.get_magic.items.weapon_pickup
  | _ ->
      let material = tile |> tile_material in
      material

let get_x_start p_x dungeon_x_length =
  if p_x - ((Magic_numbers.x_length - 1) / 2) < 0 then 0
  else if
    p_x + ((Magic_numbers.x_length - 1) / 2) + 1 > dungeon_x_length
  then dungeon_x_length - Magic_numbers.x_length
  else p_x - ((Magic_numbers.x_length - 1) / 2)

let get_x_end p_x dungeon_x_length =
  if p_x + ((Magic_numbers.x_length - 1) / 2) + 1 > dungeon_x_length
  then dungeon_x_length
  else if p_x - ((Magic_numbers.x_length - 1) / 2) < 0 then
    Magic_numbers.x_length
  else p_x + ((Magic_numbers.x_length - 1) / 2)

let get_y_start p_y dungeon_y_length =
  if p_y - ((Magic_numbers.y_length - 1) / 2) < 0 then 0
  else if
    p_y + ((Magic_numbers.y_length - 1) / 2) + 1 > dungeon_y_length
  then dungeon_y_length - Magic_numbers.y_length
  else p_y - ((Magic_numbers.y_length - 1) / 2)

let get_y_end p_y dungeon_y_length =
  if p_y + ((Magic_numbers.y_length - 1) / 2) + 1 > dungeon_y_length
  then dungeon_y_length
  else if p_y - ((Magic_numbers.y_length - 1) / 2) < 0 then
    Magic_numbers.y_length
  else p_y + ((Magic_numbers.y_length - 1) / 2)

let get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length =
  let x_start = get_x_start p_x dungeon_x_length in
  let x_end = get_x_end p_x dungeon_x_length in
  let y_start = get_y_start p_y dungeon_y_length in
  let y_end = get_y_end p_y dungeon_y_length in
  (x_start, x_end, y_start, y_end)

let determine_npc_texture npc = Spriteanimation.get_sprite npc

let determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon =
  if (x, y) = (p_x, p_y) then Spriteanimation.get_sprite "player"
  else if Hashtbl.find_opt dungeon_cells (x, y) = None then
    !Magic_numbers.get_magic.darkness
  else if (x, y) = get_start dungeon then
    !Magic_numbers.get_magic.entrance
  else if get_exit dungeon = (x, y) then !Magic_numbers.get_magic.exit
  else determine_color (Hashtbl.find dungeon_cells (x, y) |> get_tile)

let render_self_square (p_x, p_y) factor x_length y_length starting_y =
  Render.render_square
    (Render.new_square
       (p_x *. 2. /. (x_length +. 2.)
       *. (x_length *. Magic_numbers.width /. 5000.))
       (starting_y
       +. p_y /. (y_length +. 2.) *. 2.
          *. (y_length *. Magic_numbers.height /. 5000.))
       (10. /. factor) (10. /. factor) "./fonts/i.png")

let render_mini_map (p_x, p_y) (x_length, y_length, factor) =
  let p_x, p_y = (p_x /. factor, p_y /. factor) in
  let starting_y =
    2. *. (1. -. (y_length *. Magic_numbers.height /. 5000.))
  in
  Render.render_square
    (Render.new_square 0. starting_y
       (x_length *. Magic_numbers.width /. 10.)
       (y_length *. Magic_numbers.height /. 10.)
       !Magic_numbers.get_magic.darkness);
  render_self_square (p_x, p_y) factor x_length y_length starting_y

let map_bound (dungeon_x_length, dungeon_y_length) =
  let greater_dimension =
    if dungeon_x_length > dungeon_y_length then dungeon_x_length
    else dungeon_y_length
  in
  let greater_dimension_x = greater_dimension = dungeon_x_length in
  if greater_dimension > 20. then
    let factor =
      if greater_dimension_x then dungeon_x_length /. 20.
      else dungeon_x_length /. 20.
    in
    (dungeon_x_length /. factor, dungeon_y_length /. factor, factor)
  else (dungeon_x_length, dungeon_y_length, 1.)

let render_dungeon_square_helper x x_start y y_start new_texture =
  Render.render_square
    (Render.new_square
       (float_of_int (x - x_start)
       /. float_of_int Magic_numbers.x_length
       *. 2.)
       (float_of_int (y - y_start)
       /. float_of_int Magic_numbers.y_length
       *. 2.)
       Magic_numbers.width Magic_numbers.height new_texture)

let render_dungeon_helper
    (x_start, x_end)
    (y_start, y_end)
    (npc_x, npc_y)
    (p_x, p_y)
    npc_name
    dungeon_cells
    dungeon =
  for x = x_start to x_end do
    for y = y_start to y_end do
      let new_texture =
        if (npc_x, npc_y) = (x, y) then determine_npc_texture npc_name
        else determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon
      in
      render_dungeon_square_helper x x_start y y_start new_texture
    done
  done

let render_dungeon (p_x, p_y) (dungeon : t) condition =
  let dungeon_cells = dungeon |> get_cells in
  let dungeon_x_length, dungeon_y_length = dungeon |> get_dimensions in
  let x_start, x_end, y_start, y_end =
    get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length
  in
  let npc_name, npc_x, npc_y =
    match (Hashtbl.find dungeon_cells (p_x, p_y)).tile.npc with
    | Some npc, dir_x, dir_y ->
        (Npc.get_npc_sprite npc, p_x + dir_x, p_y + dir_y)
    | _ -> ("nameless", -1, -1)
  in
  render_dungeon_helper (x_start, x_end) (y_start, y_end) (npc_x, npc_y)
    (p_x, p_y) npc_name dungeon_cells dungeon;
  render_mini_map
    (float_of_int p_x, float_of_int p_y)
    (map_bound
       (float_of_int dungeon_x_length, float_of_int dungeon_y_length));
  if condition then render_npc_speech p_x p_y dungeon_cells

let get_magic_numbers d = d.magic_numbers
