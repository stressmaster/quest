type t = {
  name : string;
  sprite : string;
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
  max_hp : int;
}

let instantiate_monster
    name
    sprite
    hitpoints
    encounter_chance
    attack_strings =
  {
    name;
    sprite;
    hitpoints;
    encounter_chance;
    attack_strings;
    max_hp = hitpoints;
  }

let get_monster_HP m = m.hitpoints

let get_monster_max_HP m = m.max_hp

let monster_of_tuple
    (encounter_chance, name, sprite, hitpoints, attack_strings) =
  {
    encounter_chance;
    name;
    sprite;
    hitpoints;
    attack_strings;
    max_hp = hitpoints;
  }

let get_monster (magic_numbers : Magic_numbers.t) =
  let total =
    List.fold_left
      (fun acc (c, _, _, _, _) -> acc + c)
      0 magic_numbers.monsters
  in
  let no = Random.int total + 1 in
  let rec find_m acc = function
    | ((h, _, _, _, _) as mon) :: t ->
        if h + acc >= no then mon |> monster_of_tuple
        else find_m (h + acc) t
    | [] -> magic_numbers.monsters |> List.hd |> monster_of_tuple
  in
  find_m 0 magic_numbers.monsters

let get_monster_string m =
  let no = Random.int (List.length m.attack_strings) in
  List.nth m.attack_strings no

let change_monster_hp m plus =
  let {
    encounter_chance;
    name;
    sprite;
    hitpoints;
    attack_strings;
    max_hp;
  } =
    m
  in
  {
    encounter_chance;
    name;
    sprite;
    hitpoints;
    attack_strings;
    max_hp = max_hp + plus;
  }
