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

val empty_item : item

val get_item_sprite : item -> string
