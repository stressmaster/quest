(** Representation of an NPC.

    This module represents the data associated with an NPC, including
    data for rendering and the stat. Includes methods for creating NPCs
    and getting NPC attributes. *)

(** abstract type representing an NPC *)
type t

(** [get_npc nth magic_numbers] is the [nth] npc of [magic_numbers] *)
val get_npc : int -> Magic_numbers.t -> t

(** [get_npc_speech npc] is the speech associated with [npc] *)
val get_npc_speech : t -> string

(** [get_npc_sprite npc] is the sprite associated with [npc] *)
val get_npc_sprite : t -> string

(** [npc_list_length magic_numbers] is the number of npcs of
    [magic_numbers] *)
val npc_list_length : Magic_numbers.t -> int
