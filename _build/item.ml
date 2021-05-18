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

let get_item_sprite i =
  match i with
  | Weapon w -> w.sprite
  | Armor a -> a.sprite
  | NoItem -> Magic_numbers.empty_item_png
