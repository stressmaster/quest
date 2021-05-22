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

type monster = {
  name : string;
  sprite : string;
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
  max_hp : int;
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

let get_monster_max_HP m = m.max_hp

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
  magic_numbers : Magic_numbers.t;
  time : int;
}

let instantiate_monster
    name
    sprite
    hitpoints
    encounter_chance
    attack_strings =
  {
    name;
    sprite;
    hitpoints;
    encounter_chance;
    attack_strings;
    max_hp = hitpoints;
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

let itemless_cell dungeon (x, y) =
  let s =
    (not (is_wall dungeon (x, y - 1)))
    && get_item dungeon (x, y - 1) = None
  in
  let w =
    (not (is_wall dungeon (x - 1, y)))
    && get_item dungeon (x - 1, y) = None
  in
  let n =
    (not (is_wall dungeon (x, y + 1)))
    && get_item dungeon (x, y + 1) = None
  in
  match s with
  | true -> (x, y - 1)
  | false -> (
      match w with
      | true -> (x - 1, y)
      | false -> (
          match n with true -> (x, y + 1) | false -> (x + 1, y)))

let drop_item dungeon (x, y) item =
  let cell =
    Hashtbl.find dungeon.cells (itemless_cell dungeon (x, y))
  in
  let tile = cell.tile in
  let path_tile =
    { tile with material = !Magic_numbers.get_magic.path; item }
  in
  Hashtbl.add dungeon.cells (x, y) { cell with tile = path_tile };
  ()

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

let get_time dungeon = dungeon.time

let get_cells dungeon = dungeon.cells

let get_tile cell = cell.tile

let tile_material tile = tile.material

let get_bound dungeon = dungeon.bound

(* [instantiate_dungeon_cells x y dungeon_cells] associates (x', y')
   with a tile that has x-cord x' and y-cord y' for 0<=x'<=[x]-1 and
   0<=y'<=[y]-1 in dungeon_cells *)
(* let instantiate_dungeon_cells x y dungeon_cells = for counter_y = 0
   to y do for counter_x = 0 to x do let tile = if counter_y = 0 ||
   counter_y = y - 1 || counter_x = 0 || counter_x = x - 1 then {
   material = (!Magic_numbers.get_magic).wall; is_wall = true; item =
   None; npc = None; } else { material =
   (!Magic_numbers.get_magic).path; is_wall = false; item = None; npc =
   None; } in Hashtbl.add dungeon_cells (counter_x, counter_y) { tile; x
   = counter_x; y = counter_y } done done *)

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
      if material = !Magic_numbers.get_magic.path then 1 else 0
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
      | Some n, _, _ ->
          let speech = Npc.get_npc_speech n in
          Font.render_font ~spacing:0.05
            (Font.new_font speech 1.1 1.2
               (!Magic_numbers.get_magic.width *. 0.5)
               (!Magic_numbers.get_magic.height *. 0.5))
      | None, _, _ -> ())

let add_npc_shadow x y dungeon_cells npc_shadow dir_x dir_y =
  match
    try Some (Hashtbl.find dungeon_cells (x, y))
    with Not_found -> None
  with
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
        let npc =
          Npc.get_npc
            (Random.int (Npc.npc_list_length !Magic_numbers.get_magic))
            !Magic_numbers.get_magic
        in
        let tile =
          {
            material = Npc.get_npc_sprite npc;
            is_wall = true;
            item = None;
            npc = (Some npc, 0, 0);
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y };
        add_npc_shadows counter_x counter_y dungeon_cells (Some npc))
      else ()
    done
  done

let add_items x y dungeon_cells id =
  for counter_y = 0 to y do
    for counter_x = 0 to x do
      if
        tile_is_path counter_x counter_y dungeon_cells
        && Random.int 20 > 18
      then
        let item_type = Random.bool () in
        let item =
          Item.create_item id item_type !Magic_numbers.get_magic
        in
        let npc =
          (Hashtbl.find dungeon_cells (counter_x, counter_y)).tile.npc
        in
        let tile =
          {
            material = Item.get_item_sprite item;
            is_wall = false;
            item = Some item;
            npc;
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done

let instantiate_dungeon_cells2 x y dungeon_cells lst id =
  let rec listsearcher dungeon_cells lst =
    match lst with
    | [] -> ()
    | h :: t ->
        let tile =
          {
            material = !Magic_numbers.get_magic.path;
            is_wall = false;
            item = None;
            npc = (None, 0, 0);
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
            material = !Magic_numbers.get_magic.wall;
            is_wall = true;
            item = None;
            npc = (None, 0, 0);
          }
        in
        Hashtbl.add dungeon_cells (counter_x, counter_y)
          { tile; x = counter_x; y = counter_y }
      else ()
    done
  done;
  if Npc.npc_list_length !Magic_numbers.get_magic > 0 then
    add_npcs x y dungeon_cells;
  add_items x y dungeon_cells id

(* [instantiate_dungeon x y] is a dungeon with [x] columns [y] rows *)
let instantiate_dungeon
    ?(seed = Bigtimer.current_time ())
    id
    x
    y
    start
    bound
    monsters
    next
    prev : t =
  Random.init seed;
  let magic_numbers =
    Magic_numbers.init (Random.int Magic_numbers.length)
  in
  Magic_numbers.update magic_numbers;
  let c = Hashtbl.create (x * y) in
  (* let ourlst = carver 0 0 x y Right [] 5 300 in *)
  let w = Walker.init_walker 0 x 0 y start in
  let ourlst = Walker.walk 2600 w in
  instantiate_dungeon_cells2 x y c ourlst id;
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
    magic_numbers;
    time = seed;
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
  | Some (Armor _) -> !Magic_numbers.get_magic.armor_pickup
  | Some (Weapon _) -> !Magic_numbers.get_magic.weapon_pickup
  | _ ->
      let material = tile |> tile_material in
      material

(* [get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length] is the
   bounds for rendering based on [(p_x, p_y) dungeon_x_length
   dungeon_y_length] *)
let get_bounds (p_x, p_y) dungeon_x_length dungeon_y_length =
  let x_start =
    if p_x - ((!Magic_numbers.get_magic.x_length - 1) / 2) < 0 then 0
    else if
      p_x + ((!Magic_numbers.get_magic.x_length - 1) / 2) + 1
      > dungeon_x_length
    then dungeon_x_length - !Magic_numbers.get_magic.x_length
    else p_x - ((!Magic_numbers.get_magic.x_length - 1) / 2)
  in
  let x_end =
    if
      p_x + ((!Magic_numbers.get_magic.x_length - 1) / 2) + 1
      > dungeon_x_length
    then dungeon_x_length
    else if p_x - ((!Magic_numbers.get_magic.x_length - 1) / 2) < 0 then
      !Magic_numbers.get_magic.x_length
    else p_x + ((!Magic_numbers.get_magic.x_length - 1) / 2)
  in
  let y_start =
    if p_y - ((!Magic_numbers.get_magic.y_length - 1) / 2) < 0 then 0
    else if
      p_y + ((!Magic_numbers.get_magic.y_length - 1) / 2) + 1
      > dungeon_y_length
    then dungeon_y_length - !Magic_numbers.get_magic.y_length
    else p_y - ((!Magic_numbers.get_magic.y_length - 1) / 2)
  in
  let y_end =
    if
      p_y + ((!Magic_numbers.get_magic.y_length - 1) / 2) + 1
      > dungeon_y_length
    then dungeon_y_length
    else if p_y - ((!Magic_numbers.get_magic.y_length - 1) / 2) < 0 then
      !Magic_numbers.get_magic.y_length
    else p_y + ((!Magic_numbers.get_magic.y_length - 1) / 2)
  in
  (x_start, x_end, y_start, y_end)

(* [determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon] is the
   texture of the tile at [(x, y)] based on [(p_x, p_y) dungeon_cells
   dungeon] *)

let determine_npc_texture npc = Spriteanimation.get_sprite npc

let determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon =
  if (x, y) = (p_x, p_y) then Spriteanimation.get_sprite "player"
  else if Hashtbl.find_opt dungeon_cells (x, y) = None then
    !Magic_numbers.get_magic.darkness
  else if (x, y) = get_start dungeon then
    !Magic_numbers.get_magic.entrance
  else if get_exit dungeon = (x, y) then !Magic_numbers.get_magic.exit
  else determine_color (Hashtbl.find dungeon_cells (x, y) |> get_tile)

let render_mini_map (p_x, p_y) (dungeon_x_length, dungeon_y_length) =
  let starting_y =
    2.
    *. (1.
       -. dungeon_y_length *. !Magic_numbers.get_magic.height /. 10.
          /. 500.)
  in
  Render.render_square
    (Render.new_square 0. starting_y
       (dungeon_x_length *. !Magic_numbers.get_magic.width /. 10.)
       (dungeon_y_length *. !Magic_numbers.get_magic.height /. 10.)
       !Magic_numbers.get_magic.darkness);

  Render.render_square
    (Render.new_square
       (p_x *. 2.
       /. (dungeon_x_length +. 2.)
       *. (dungeon_x_length *. !Magic_numbers.get_magic.width /. 10.)
       /. 500.)
       (starting_y
       +. p_y
          /. (dungeon_y_length +. 2.)
          *. 2.
          *. (dungeon_y_length *. !Magic_numbers.get_magic.height /. 10.)
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
  let npc_name, x_npc, y_npc =
    match (Hashtbl.find dungeon_cells (p_x, p_y)).tile.npc with
    | Some npc, dir_x, dir_y ->
        (Npc.get_npc_sprite npc, p_x + dir_x, p_y + dir_y)
    | _ -> ("nameless", -1, -1)
  in
  for x = x_start to x_end do
    for y = y_start to y_end do
      let new_texture =
        if (x_npc, y_npc) = (x, y) then determine_npc_texture npc_name
        else determine_texture (x, y) (p_x, p_y) dungeon_cells dungeon
      in
      Render.render_square
        (Render.new_square
           (float_of_int (x - x_start)
           /. float_of_int !Magic_numbers.get_magic.x_length
           *. 2.)
           (float_of_int (y - y_start)
           /. float_of_int !Magic_numbers.get_magic.y_length
           *. 2.)
           !Magic_numbers.get_magic.width
           !Magic_numbers.get_magic.height new_texture)
    done
  done;
  render_mini_map
    (float_of_int p_x, float_of_int p_y)
    (float_of_int dungeon_x_length, float_of_int dungeon_y_length);
  (* if Render_stack.stack_peek () != Render_stack.SpiralRender then *)
  if condition then render_npc_speech p_x p_y dungeon_cells

let get_magic_numbers d = d.magic_numbers

(* Render_stack.stack_push Render_stack.DungeonRender *)
