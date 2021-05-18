type t = {
  id : string;
  sprites : string list;
  ticker : int ref;
}

let init_animation id sprites = { id; sprites; ticker = ref 0 }

let get_sprite t = List.nth t.sprites !(t.ticker)

let animation_ticker t =
  t.ticker := (!(t.ticker) + 1) mod List.length t.sprites
