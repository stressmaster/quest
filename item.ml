type item_stats = {
  sprite : string;
  name : string;
  depth : int;
  modifier : int;
}

type item =
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
    "legendayr";
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
  [ "paper"; "wood"; "styrofoam"; "cardboard"; "tin foil"; "flint" ]

let tier_two_materials =
  [ "iron"; "steel"; "bronze"; "copper"; "metal" ]

let tier_three_materials =
  [ "tungsten"; "diamond"; "titanium"; "mithril"; "depleted uranium" ]

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
  ]

let tier_two_weapons = [ "sword"; "longsword"; "spear" ]

let tier_three_weapons =
  [
    "halberd"; "scimitar"; "claymore"; "warhammer"; "kunai"; "chainsaw";
  ]

let get_item_sprite i =
  match i with
  | Weapon w -> w.sprite
  | Armor a -> a.sprite
  | NoItem -> Magic_numbers.empty_item_png

let generate_name tier =
  let prefix =
    if tier = 3. then
      List.nth tier_three_prefixes
        (Random.int (List.length tier_three_prefixes))
    else if tier = 1. then
      List.nth tier_two_prefixes
        (Random.int (List.length tier_two_prefixes))
    else
      List.nth tier_one_prefixes
        (Random.int (List.length tier_one_prefixes))
  in
  let material =
    if tier = 3. then
      List.nth tier_three_materials
        (Random.int (List.length tier_three_materials))
    else if tier = 1. then
      List.nth tier_two_materials
        (Random.int (List.length tier_two_materials))
    else
      List.nth tier_one_prefixes
        (Random.int (List.length tier_one_materials))
  in
  let weapon =
    if tier = 3. then
      List.nth tier_three_weapons
        (Random.int (List.length tier_three_weapons))
    else if tier = 1. then
      List.nth tier_two_weapons
        (Random.int (List.length tier_two_weapons))
    else
      List.nth tier_one_weapons
        (Random.int (List.length tier_one_weapons))
  in
  prefix ^ material ^ weapon

let create_item depth itype =
  let tier =
    let rand = Random.float 1. in
    if rand > 0.9 then 3. else if rand > 0.5 then 1. else 0.5
  in
  let n = generate_name tier in
  if itype = true then
    Weapon
      {
        sprite = Magic_numbers.monster;
        name = n;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
  else
    Armor
      {
        sprite = Magic_numbers.monster;
        name = n;
        depth;
        modifier = int_of_float (float_of_int depth *. tier);
      }
