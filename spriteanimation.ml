type animation = {
  sprites : string list;
  ticker : int ref;
}

type id = string

let animation_map = ref []

let init_animations (id_sprites_lst : (id * string list) list) =
  let add_to_animations h =
    animation_map :=
      (fst h, { sprites = snd h; ticker = ref 0 }) :: !animation_map
  in
  List.iter add_to_animations id_sprites_lst

let get_sprite id =
  let { sprites; ticker } = List.assoc id !animation_map in
  List.nth sprites !ticker

let step_animation () =
  let step_ticker h =
    let id, { sprites; ticker } = h in
    ticker := (!ticker + 1) mod List.length sprites;
    h
  in
  animation_map := List.map step_ticker !animation_map
