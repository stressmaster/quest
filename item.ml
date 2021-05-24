type item_stats = {
  sprite : string;
  name : string;
  depth : int;
  modifier : int;
}

type t =
  | Weapon of item_stats
  | Armor of item_stats
  | NoItem

let create_item_hard itype sprite name depth modifier =
  if itype = "weapon" then Weapon { sprite; name; depth; modifier }
  else if itype = "armor" then Armor { sprite; name; depth; modifier }
  else NoItem

let get_item_type t =
  match t with
  | Weapon w -> "weapon"
  | Armor a -> "armor"
  | NoItem -> "none"

let empty_item = NoItem

let get_item_sprite i =
  match i with
  | Weapon w -> w.sprite
  | Armor a -> a.sprite
  | NoItem -> !Magic_numbers.get_magic.items.empty_item

let get_item_depth i =
  match i with Weapon w -> w.depth | Armor a -> a.depth | NoItem -> 0

let get_item_name i =
  match i with Weapon w -> w.name | Armor a -> a.name | NoItem -> ""

let get_item_modifier i =
  match i with
  | Weapon w -> w.modifier
  | Armor a -> a.modifier
  | NoItem -> 0

let list_rand_elt lst = List.nth lst (Random.int (List.length lst))

let determine_prefix tier (dereffed_magic_numbers : Magic_numbers.t) =
  if tier = 4. then
    list_rand_elt
      dereffed_magic_numbers.items.plurals.tier_three_prefixes
  else if tier = 1. then
    list_rand_elt dereffed_magic_numbers.items.plurals.tier_two_prefixes
  else
    list_rand_elt dereffed_magic_numbers.items.plurals.tier_one_prefixes

let determine_material tier (dereffed_magic_numbers : Magic_numbers.t) =
  if tier = 4. then
    list_rand_elt
      dereffed_magic_numbers.items.plurals.tier_three_materials
  else if tier = 1. then
    list_rand_elt
      dereffed_magic_numbers.items.plurals.tier_two_materials
  else
    list_rand_elt
      dereffed_magic_numbers.items.plurals.tier_one_materials

let determine_base itype tier (dereffed_magic_numbers : Magic_numbers.t)
    =
  if itype = true then
    if tier = 4. then
      list_rand_elt
        dereffed_magic_numbers.items.plurals.tier_three_weapons
    else if tier = 1. then
      list_rand_elt
        dereffed_magic_numbers.items.plurals.tier_two_weapons
    else
      list_rand_elt
        dereffed_magic_numbers.items.plurals.tier_one_weapons
  else if tier = 4. then
    list_rand_elt dereffed_magic_numbers.items.plurals.tier_three_armors
  else if tier = 1. then
    list_rand_elt dereffed_magic_numbers.items.plurals.tier_two_armors
  else
    list_rand_elt dereffed_magic_numbers.items.plurals.tier_one_armors

let generate_name tier itype (dereffed_magic_numbers : Magic_numbers.t)
    =
  let prefix = determine_prefix tier dereffed_magic_numbers in

  let material = determine_material tier dereffed_magic_numbers in

  let base = determine_base itype tier dereffed_magic_numbers in

  prefix ^ " " ^ material ^ " " ^ base

let sprite_of_tier t itype =
  match (t, itype) with
  | 4., true -> !Magic_numbers.get_magic.items.tier_three_weapon
  | 4., false -> !Magic_numbers.get_magic.items.tier_three_armor
  | 1., true -> !Magic_numbers.get_magic.items.tier_two_weapon
  | 1., false -> !Magic_numbers.get_magic.items.tier_two_armor
  | 0.5, true -> !Magic_numbers.get_magic.items.tier_one_weapon
  | 0.5, false -> !Magic_numbers.get_magic.items.tier_one_armor
  | _ -> !Magic_numbers.get_magic.items.tier_one_armor

let create_weapon depth tier dereffed_magic_numbers =
  Weapon
    {
      sprite = sprite_of_tier tier true;
      name =
        "Lvl " ^ string_of_int depth ^ " "
        ^ generate_name tier true dereffed_magic_numbers;
      depth;
      modifier =
        int_of_float
          (float_of_int (Random.int depth)
          +. (tier *. float_of_int depth));
    }

let create_item depth itype (dereffed_magic_numbers : Magic_numbers.t) =
  let tier =
    let rand = Random.float 1. in
    if rand > 0.9 then 4. else if rand > 0.5 then 1. else 0.5
  in
  if itype = true then create_weapon depth tier dereffed_magic_numbers
  else
    Armor
      {
        sprite = sprite_of_tier tier false;
        name =
          "Lvl " ^ string_of_int depth ^ " "
          ^ generate_name tier false dereffed_magic_numbers;
        depth;
        modifier =
          int_of_float
            (float_of_int (Random.int depth)
            +. (tier *. float_of_int depth));
      }
