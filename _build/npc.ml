type t = {
  speech : string;
  sprite : string;
}

let npc_list =
  [
    {
      speech = "That goblin has stolen my slippers!";
      sprite = "monster.png";
    };
    { speech = "Where am I?"; sprite = "goblin_1.png" };
  ]

let get_npc n = List.nth npc_list (n / List.length npc_list)

let get_npc_speech npc = npc.speech

let get_npc_sprite npc = npc.sprite
