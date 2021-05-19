type t = {
  speech : string;
  sprite : string;
}

let npc_list =
  [
    {
      speech = "That goblin has stolen my slippers!";
      sprite = Magic_numbers.monster;
    };
    { speech = "Where am I?"; sprite = Magic_numbers.goblin_1 };
    { speech = "Baka..."; sprite = Magic_numbers.monster3 };
    { speech = "Where am I?"; sprite = Magic_numbers.monster };
  ]

let get_npc n = List.nth npc_list (n / List.length npc_list)

let get_npc_speech npc = npc.speech

let get_npc_sprite npc = npc.sprite
