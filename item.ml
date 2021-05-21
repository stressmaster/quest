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

let empty_item = NoItem

let get_item_sprite i =
  match i with
  | Weapon w -> w.sprite
  | Armor a -> a.sprite
  | NoItem -> !Magic_numbers.get_magic.empty_item

let get_item_name i =
  match i with Weapon w -> w.name | Armor a -> a.name | NoItem -> ""

let get_item_modifier i =
  match i with
  | Weapon w -> w.modifier
  | Armor a -> a.modifier
  | NoItem -> 0

let list_rand_elt lst = List.nth lst (Random.int (List.length lst))

let generate_name tier itype (dereffed_magic_numbers : Magic_numbers.t)
    =
  let prefix =
    if tier = 3. then
      list_rand_elt dereffed_magic_numbers.tier_three_prefixes
    else if tier = 1. then
      list_rand_elt dereffed_magic_numbers.tier_two_prefixes
    else list_rand_elt dereffed_magic_numbers.tier_one_prefixes
  in
  let material =
    if tier = 3. then
      list_rand_elt dereffed_magic_numbers.tier_three_materials
    else if tier = 1. then
      list_rand_elt dereffed_magic_numbers.tier_two_materials
    else list_rand_elt dereffed_magic_numbers.tier_one_materials
  in
  let base =
    if itype = true then
      if tier = 3. then
        list_rand_elt dereffed_magic_numbers.tier_three_weapons
      else if tier = 1. then
        list_rand_elt dereffed_magic_numbers.tier_two_weapons
      else list_rand_elt dereffed_magic_numbers.tier_one_weapons
    else if tier = 3. then
      list_rand_elt dereffed_magic_numbers.tier_three_armors
    else if tier = 1. then
      list_rand_elt dereffed_magic_numbers.tier_two_armors
    else list_rand_elt dereffed_magic_numbers.tier_one_armors
  in
  prefix ^ " " ^ material ^ " " ^ base

let create_item depth itype (dereffed_magic_numbers : Magic_numbers.t) =
  let tier =
    let rand = Random.float 1. in
    if rand > 0.9 then 3. else if rand > 0.5 then 1. else 0.5
  in
  if itype = true then
    Weapon
      {
        sprite = !Magic_numbers.get_magic.tier_one_weapon;
        name = generate_name tier true dereffed_magic_numbers;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
  else
    Armor
      {
        sprite = !Magic_numbers.get_magic.tier_one_armor;
        name = generate_name tier false dereffed_magic_numbers;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
