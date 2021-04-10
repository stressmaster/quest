open Yojson.Basic.Util

type t = {
  dungeons : Dungeon.t list;
  start_dungeon : Dungeon.t;
}

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
    (json |> member "end-x" |> to_int, json |> member "end-y" |> to_int)
    (json |> member "bound" |> to_int)
    (json |> member "monsters" |> to_list |> List.map monster_of_json)
    (json |> member "next" |> to_int)
    (json |> member "prev" |> to_int)

let from_json json =
  {
    dungeons =
      json |> member "dungeons" |> to_list |> List.map dungeon_of_json;
    start_dungeon =
      Dungeon.instantiate_dungeon 1 20 50 (1, 1) (18, 48) 10 [] 0 9;
  }

let start_room game = game.start_dungeon
