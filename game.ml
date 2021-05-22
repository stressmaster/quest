open Yojson.Basic.Util

type t = { dungeons : Dungeon.t list (* start_room : Dungeon.t; *) }

type monster = {
  name : string;
  sprite : string;
  hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
}

let monster_of_json json =
  Dungeon.instantiate_monster
    (json |> member "name" |> to_string)
    (json |> member "sprite" |> to_string)
    (json |> member "hitpoints" |> to_int)
    (json |> member "chance" |> to_int)
    (json |> member "attacks" |> to_list |> List.map to_string)

(* soon obsolete? *)

let dungeon_of_json json =
  Dungeon.instantiate_dungeon
    (json |> member "id" |> to_int)
    (json |> member "x-dim" |> to_int)
    (json |> member "y-dim" |> to_int)
    ( json |> member "start-x" |> to_int,
      json |> member "start-y" |> to_int )
    (json |> member "bound" |> to_int)
    (json |> member "monsters" |> to_list |> List.map monster_of_json)
    (json |> member "next" |> to_int)
    (json |> member "prev" |> to_int)

let from_json json =
  {
    dungeons =
      json |> member "dungeons" |> to_list |> List.map dungeon_of_json;
  }

(* soon obsolete? *)

let monster_set i =
  Yojson.Basic.from_file "monsters.json"
  |> member ("set" ^ string_of_int i)
  |> to_list
  |> List.map monster_of_json

let dungeon_of_room save =
  let id = save |> member "id" |> to_int in
  Dungeon.instantiate_dungeon_with_seed id
    (save |> member "x-dim" |> to_int)
    (save |> member "y-dim" |> to_int)
    ( save |> member "xstart" |> to_int,
      save |> member "ystart" |> to_int )
    20
    (monster_set (id mod 5))
    (id + 1) (id - 1)
    (save |> member "time" |> to_int)

let save_json json =
  {
    dungeons =
      json |> member "rooms" |> to_list |> List.rev_map dungeon_of_room;
  }

let start_room game =
  List.find (fun g -> Dungeon.get_id g = 0) game.dungeons

let next_dungeon game dungeon =
  let next_id = Dungeon.get_id dungeon + 1 in
  try List.find (fun g -> Dungeon.get_id g = next_id) game.dungeons
  with Not_found ->
    let next_id = Dungeon.get_id dungeon + 1 in
    let xdim = 11 + Random.int 20 in
    let ydim = 11 + Random.int 20 in
    let ourlist =
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
    in
    Dungeon.instantiate_dungeon next_id xdim ydim
      (List.nth ourlist (Random.int 9))
      20
      (monster_set (next_id mod 5))
      (next_id + 1) (next_id - 1)

let prev_dungeon game dungeon =
  (* let prev_dungeon_id = Dungeon.get_prev dungeon in *)
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
      let (time : int) = Dungeon.get_time h in
      let id = Dungeon.get_id h in
      let room =
        `Assoc
          [
            ("id", `Int id);
            ("xdim", `Int xdim);
            ("ydim", `Int ydim);
            ("xstart", `Int xstart);
            ("ystart", `Int ystart);
            ("time", `Int time);
          ]
      in
      room_maker t (room :: acc)
  | _ -> acc

let json_maker exists room_number game : Yojson.Basic.t =
  let rooms = room_maker game.dungeons [] in
  let ourjson =
    `Assoc
      [
        ("exists", `Bool exists);
        ("number_rooms", `Int room_number);
        ("rooms", `List rooms);
      ]
  in
  ourjson

let update_file ourjson = Yojson.Basic.to_file "save.json" ourjson
