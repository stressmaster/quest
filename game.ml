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
      json |> member "dungeons" |> to_list |> List.map dungeon_of_json
      (* start_room = Dungeon.instantiate_dungeon 0 12 50 (1, 1) (10,
         48) 10 [] 0 3; *);
  }

let start_room game =
  (* game.start_room *)
  List.find (fun g -> Dungeon.get_id g = 0) game.dungeons

let next_dungeon game dungeon =
  let next_id = Dungeon.get_id dungeon + 1 in
  try List.find (fun g -> Dungeon.get_id g = next_id) game.dungeons
  with Not_found ->
    (* something about depth and tiles here*)
    let next_id = Dungeon.get_id dungeon + 1 in
    let xdim = 11 + Random.int 20 in
    let ydim = 11 + Random.int 20 in
    Dungeon.instantiate_dungeon next_id xdim ydim (1, 1) 20
      (Dungeon.get_monsters dungeon)
      (next_id + 1) (next_id - 1)

let prev_dungeon game dungeon =
  (* let prev_dungeon_id = Dungeon.get_prev dungeon in *)
  let prev_id = Dungeon.get_id dungeon - 1 in
  List.find (fun g -> Dungeon.get_id g = prev_id) game.dungeons

let add_to_game game dungeon = { dungeons = dungeon :: game.dungeons }

let game_depth game = List.length game
