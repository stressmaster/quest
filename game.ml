open Yojson.Basic.Util

type t = { dungeons : Dungeon.t list (* start_room : Dungeon.t; *) }

(* soon obsolete? *)

let dungeon_of_json json =
  Dungeon.instantiate_dungeon
    (json |> member "id" |> to_int)
    (json |> member "x-dim" |> to_int)
    (json |> member "y-dim" |> to_int)
    ( json |> member "start-x" |> to_int,
      json |> member "start-y" |> to_int )
    (json |> member "bound" |> to_int)
    (json |> member "next" |> to_int)
    (json |> member "prev" |> to_int)

let from_json json =
  { dungeons = [ json |> member "dungeon" |> dungeon_of_json ] }

(* soon obsolete? *)

let dungeon_of_room save =
  let id = save |> member "id" |> to_int in
  Dungeon.instantiate_dungeon
    ~seed:(save |> member "time" |> to_int)
    id
    (save |> member "xdim" |> to_int)
    (save |> member "ydim" |> to_int)
    ( save |> member "xstart" |> to_int,
      save |> member "ystart" |> to_int )
    20 (id + 1) (id - 1)

let item_of_json json =
  let typestring = json |> member "type" |> to_string in
  let oursprite = json |> member "sprite" |> to_string in
  let ourname = json |> member "name" |> to_string in
  let ourdepth = json |> member "depth" |> to_int in
  let ourmodifier = json |> member "modifier" |> to_int in
  Item.create_item_hard typestring oursprite ourname ourdepth
    ourmodifier

let save_json json =
  {
    dungeons =
      json |> member "rooms" |> to_list |> List.rev_map dungeon_of_room;
  }

let start_room game =
  List.find (fun g -> Dungeon.get_id g = 1) game.dungeons

let nth_room game n =
  List.find (fun g -> Dungeon.get_id g = n) game.dungeons

let last_room game = List.hd game.dungeons

let coor_maker xdim ydim =
  [
    (1, 1);
    (xdim - 2, ydim - 2);
    (xdim - 2, 1);
    (2, ydim - 2);
    (xdim / 2, ydim / 2);
    (xdim / 2, ydim - 2);
    (xdim / 2, 1);
    (1, ydim / 2);
    (xdim - 2, ydim / 2);
  ]

let next_dungeon game dungeon =
  let next_id = Dungeon.get_id dungeon + 1 in
  try List.find (fun g -> Dungeon.get_id g = next_id) game.dungeons
  with Not_found ->
    let next_id = Dungeon.get_id dungeon + 1 in
    let xdim = 50 + Random.int 80 in
    let ydim = 50 + Random.int 80 in
    let ourlist = coor_maker xdim ydim in
    Dungeon.instantiate_dungeon next_id xdim ydim
      (List.nth ourlist (Random.int 9))
      35 (next_id + 1) (next_id - 1)

let prev_dungeon game dungeon =
  let prev_id = Dungeon.get_id dungeon - 1 in
  List.find (fun g -> Dungeon.get_id g = prev_id) game.dungeons

let add_to_game game dungeon = { dungeons = dungeon :: game.dungeons }

let game_depth game = List.length game.dungeons

let rec room_maker dungeonlist acc : Yojson.Basic.t list =
  match dungeonlist with
  | h :: t ->
      let xdim = fst (Dungeon.get_dimensions h) in
      let ydim = snd (Dungeon.get_dimensions h) in
      let xstart = fst (Dungeon.get_start h) in
      let ystart = snd (Dungeon.get_start h) in
      let room =
        `Assoc
          [
            ("id", `Int (Dungeon.get_id h));
            ("xdim", `Int xdim);
            ("ydim", `Int ydim);
            ("xstart", `Int xstart);
            ("ystart", `Int ystart);
            ("time", `Int (Dungeon.get_time h));
          ]
      in
      room_maker t (room :: acc)
  | _ -> acc

let json_of_item item =
  `Assoc
    [
      ("type", `String (Item.get_item_type item));
      ("sprite", `String (Item.get_item_sprite item));
      ("name", `String (Item.get_item_name item));
      ("depth", `Int (Item.get_item_depth item));
      ("modifier", `Int (Item.get_item_modifier item));
    ]

let json_maker level hea liv e locx locy curid rnum cure e_b wn ar game
    : Yojson.Basic.t =
  let rooms = room_maker game.dungeons [] in
  `Assoc
    [
      ("exists", `Bool e);
      ("locationx", `Int locx);
      ("locationy", `Int locy);
      ("current_id", `Int curid);
      ("number_rooms", `Int rnum);
      ("lives", `Int liv);
      ("level", `Int level);
      ("health", `Int hea);
      ("rooms", `List rooms);
      ("current_exp", `Int cure);
      ("exp_bound", `Int e_b);
      ("current_weapon", json_of_item wn);
      ("current_armor", json_of_item ar);
    ]

let update_file ourjson = Yojson.Basic.to_file "save.json" ourjson

let reset_save : Yojson.Basic.t = `Assoc [ ("exists", `Bool false) ]
