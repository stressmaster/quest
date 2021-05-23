open Yojson.Basic.Util

let yojson = Yojson.Basic.from_file "magic_numbers_one.json"

let magic_themes = yojson |> member "magic_themes" |> to_list

let textures = yojson |> member "texture_list"

let length = List.length magic_themes

type t = {
  wall : string;
  path : string;
  entrance : string;
  exit : string;
  player : string;
  timer : string;
  darkness : string;
  empty_item : string;
  weapon_pickup : string;
  armor_pickup : string;
  tier_one_weapon : string;
  tier_one_armor : string;
  animations : (string * string list) list;
  w : int;
  square_height : height:float -> float;
  h : int;
  x_length : int;
  y_length : int;
  health : int;
  width : float;
  height : float;
  npcs : (string * string) list;
  tier_one_prefixes : string list;
  tier_two_prefixes : string list;
  tier_three_prefixes : string list;
  tier_one_materials : string list;
  tier_two_materials : string list;
  tier_three_materials : string list;
  tier_one_weapons : string list;
  tier_two_weapons : string list;
  tier_three_weapons : string list;
  tier_one_armors : string list;
  tier_two_armors : string list;
  tier_three_armors : string list;
  monsters : (int * string * string * int * string list) list;
  texture_list : string list;
}

let json_to_string json field = json |> member field |> to_string

let json_to_int json field = json |> member field |> to_int

let json_to_string_list json field =
  List.map to_string (json |> member field |> to_list)

let json_to_string_list_lite json = List.map to_string (json |> to_list)

let to_string_list json = List.map to_string json

let json_to_animation lst =
  let lst = lst |> to_list |> to_string_list in
  (List.hd lst, List.tl lst)

let json_to_animations json field =
  List.map json_to_animation (json |> member field |> to_list)

let to_npc json_npc =
  ( json_npc |> member "speech" |> to_string,
    json_npc |> member "sprite" |> to_string )

let json_to_npcs json field =
  List.map to_npc (json |> member field |> to_list)

let json_to_monster_textures json = json |> member "sprite" |> to_string

let json_to_npcs_textures json = json |> member "sprite" |> to_string

let json_to_monster json =
  ( json |> member "chance" |> to_int,
    json |> member "name" |> to_string,
    json |> member "sprite" |> to_string,
    json |> member "hitpoints" |> to_int,
    json |> member "attacks" |> to_list |> List.map to_string )

let magic_theme_to_magic_numbers magic_theme =
  let w = json_to_int magic_theme "w" in
  let h = json_to_int magic_theme "h" in
  let x_length = json_to_int magic_theme "x_length" in
  let y_length = json_to_int magic_theme "y_length" in
  let wall = json_to_string magic_theme "wall" in
  let path = json_to_string magic_theme "path" in
  let entrance = json_to_string magic_theme "entrance" in
  let exit = json_to_string magic_theme "exit" in
  let player = json_to_string magic_theme "player" in
  let darkness = json_to_string magic_theme "darkness" in
  let timer = json_to_string magic_theme "timer" in
  let empty_item = json_to_string magic_theme "empty_item" in
  let weapon_pickup = json_to_string magic_theme "weapon_pickup" in
  let armor_pickup = json_to_string magic_theme "armor_pickup" in
  let tier_one_weapon = json_to_string magic_theme "tier_one_weapon" in
  let tier_one_armor = json_to_string magic_theme "tier_one_armor" in
  let monsters =
    magic_theme |> member "monsters" |> to_list
    |> List.map json_to_monster
  in
  let npcs = json_to_npcs magic_theme "npcs" in
  {
    wall;
    path;
    entrance;
    exit;
    player;
    darkness;
    timer;
    empty_item;
    weapon_pickup;
    armor_pickup;
    tier_one_weapon;
    tier_one_armor;
    monsters;
    w;
    h;
    x_length;
    y_length;
    width = float_of_int w /. float_of_int x_length;
    height = float_of_int h /. float_of_int y_length;
    texture_list = json_to_string_list_lite textures;
    animations = json_to_animations magic_theme "animations";
    health = json_to_int magic_theme "health";
    square_height = (fun ~height -> float_of_int h /. height);
    npcs;
    tier_one_prefixes =
      json_to_string_list magic_theme "tier_one_prefixes";
    tier_two_prefixes =
      json_to_string_list magic_theme "tier_two_prefixes";
    tier_three_prefixes =
      json_to_string_list magic_theme "tier_three_prefixes";
    tier_one_materials =
      json_to_string_list magic_theme "tier_one_materials";
    tier_two_materials =
      json_to_string_list magic_theme "tier_two_materials";
    tier_three_materials =
      json_to_string_list magic_theme "tier_three_materials";
    tier_one_weapons =
      json_to_string_list magic_theme "tier_one_weapons";
    tier_two_weapons =
      json_to_string_list magic_theme "tier_two_weapons";
    tier_three_weapons =
      json_to_string_list magic_theme "tier_three_weapons";
    tier_one_armors = json_to_string_list magic_theme "tier_one_armors";
    tier_two_armors = json_to_string_list magic_theme "tier_two_armors";
    tier_three_armors =
      json_to_string_list magic_theme "tier_three_armors";
  }

let init n =
  let magic_theme = List.nth magic_themes n in

  magic_theme_to_magic_numbers magic_theme

let magic_numbers = ref (init 1)

let update new_magic_numbers = magic_numbers := new_magic_numbers

let get_magic = magic_numbers
