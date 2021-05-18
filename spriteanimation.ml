type animation = {
  sprites : string list;
  ticker : int ref;
}

type id = string

type animations = (id * animation) list

let animation_map = ref []

let rec init_animation (id_sprites_lst : (id * string list) list) =
  match id_sprites_lst with
  | h :: t ->
      animation_map :=
        (fst h, { sprites = snd h; ticker = ref 0 }) :: !animation_map;
      init_animation t
  | [] -> ()

let get_sprite id =
  let { sprites; ticker } = List.assoc id !animation_map in
  List.nth sprites !ticker

let step_animation () =
  let rec step_helper lst =
    match lst with
    | h :: t ->
        let id, { sprites; ticker } = h in
        ticker := (!ticker + 1) mod List.length sprites;
        h :: step_helper t
    | [] -> []
  in
  animation_map := step_helper !animation_map
