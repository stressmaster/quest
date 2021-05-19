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

let tier_one_prefixes =
  [
    "flimsy";
    "weak";
    "shoddy";
    "impotent";
    "puny";
    "feeble";
    "scrappy";
    "pathetic";
    "brittle";
    "lowly";
    "lousy";
  ]

let tier_two_prefixes =
  [
    "mediocre";
    "average";
    "typical";
    "unassuming";
    "plain";
    "unexceptional";
    "decent";
    "middling";
    "ordinary";
    "standard";
  ]

let tier_three_prefixes =
  [
    "epic";
    "legendary";
    "high quality";
    "amazing";
    "superb";
    "mediocre";
    "exceptional";
    "radiant";
    "unreal";
    "phenomenal";
    "mythical";
  ]

let tier_one_materials =
  [
    "paper";
    "wood";
    "styrofoam";
    "cardboard";
    "tin foil";
    "flint";
    "cloth";
  ]

let tier_two_materials =
  [ "iron"; "steel"; "bronze"; "copper"; "metal" ]

let tier_three_materials =
  [
    "tungsten";
    "diamond";
    "titanium";
    "mithril";
    "depleted uranium";
    "carbon fiber";
  ]

let tier_one_weapons =
  [
    "butter knife";
    "stick";
    "fork";
    "spoon";
    "pencil";
    "clothes hanger";
    "mop";
    "broom";
    "broken bottle";
    "scissors";
  ]

let tier_two_weapons =
  [ "sword"; "longsword"; "spear"; "blade"; "dagger"; "knife" ]

let tier_three_weapons =
  [
    "halberd";
    "scimitar";
    "claymore";
    "warhammer";
    "kunai";
    "chainsaw";
    "zweihander";
    "saw cleaver";
    "lance of longinus";
    "katana";
  ]

let tier_one_armors =
  [ "covering"; "apron"; "smock"; "tank top"; "rags"; "jacket" ]

let tier_two_armors =
  [ "vest"; "suit"; "chestplate"; "plate mail"; "cuirass"; "hauberk" ]

let tier_three_armors =
  [ "armor"; "power armor"; "mecha suit"; "oyoroi"; "exoskeleton" ]

let get_item_sprite i =
  match i with
  | Weapon w -> w.sprite
  | Armor a -> a.sprite
  | NoItem -> Magic_numbers.empty_item_png

let get_item_name i =
  match i with Weapon w -> w.name | Armor a -> a.name | NoItem -> ""

let get_item_modifier i =
  match i with
  | Weapon w -> w.modifier
  | Armor a -> a.modifier
  | NoItem -> 0

let list_rand_elt lst = List.nth lst (Random.int (List.length lst))

let generate_name tier itype =
  let prefix =
    if tier = 3. then list_rand_elt tier_three_prefixes
    else if tier = 1. then list_rand_elt tier_two_prefixes
    else list_rand_elt tier_one_prefixes
  in
  let material =
    if tier = 3. then list_rand_elt tier_three_materials
    else if tier = 1. then list_rand_elt tier_two_materials
    else list_rand_elt tier_one_materials
  in
  let base =
    if itype = true then
      if tier = 3. then list_rand_elt tier_three_weapons
      else if tier = 1. then list_rand_elt tier_two_weapons
      else list_rand_elt tier_one_weapons
    else if tier = 3. then list_rand_elt tier_three_armors
    else if tier = 1. then list_rand_elt tier_two_armors
    else list_rand_elt tier_one_armors
  in
  prefix ^ " " ^ material ^ " " ^ base

let create_item depth itype =
  let tier =
    let rand = Random.float 1. in
    if rand > 0.9 then 3. else if rand > 0.5 then 1. else 0.5
  in
  if itype = true then
    Weapon
      {
        sprite = Magic_numbers.tier_one_weapon;
        name = generate_name tier true;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
  else
    Armor
      {
        sprite = Magic_numbers.tier_one_armor;
        name = generate_name tier false;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
