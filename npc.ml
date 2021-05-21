type t = {
  speech : string;
  sprite : string;
}

let get_npc_speech npc = npc.speech

let get_npc_sprite npc = npc.sprite

let get_npc n (dereffed_magic_numbers : Magic_numbers.t) =
  let npc = List.nth dereffed_magic_numbers.npcs n in
  { speech = fst npc; sprite = snd npc }

let npc_list_length (dereffed_magic_numbers : Magic_numbers.t) =
  List.length dereffed_magic_numbers.npcs
