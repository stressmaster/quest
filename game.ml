open Yojson.Basic.Util

type t = { dungeons : Dungeon.t list }

type monster = {
  name : string;
  sprite : string;
  hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
}

let monster_of_json json =
  Dungeon.instantiate_monster (name = json |> member "name" |> to_string);
  sprite = json |> member "sprite" |> to_string;
  hitpoints = json |> member "hitpoints" |> to_int;
  encounter_chance = json |> member "chance" |> to_int;
  attack_strings = json |> member "attacks" |> to_list
  |> List.map to_string

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
  }
