(** Representation of the game.

    This module represents the data needed to maintain and render the
    game, including data about the dungeons in the game. Includes
    methods for converting JSON to a game saving the game to JSON. *)

(** an abstract type representing a game *)
type t

(** [from_json json] is a game based on [json] *)
val from_json : Yojson.Basic.t -> t

(** [from_json save_file] is a game based on [save_file] *)
val save_json : Yojson.Basic.t -> t

(** [start_room game] is the first dungeon in [game] *)
val start_room : t -> Dungeon.t

(** [last_room game] is the last dungeon in [game] *)
val last_room : t -> Dungeon.t

(** [nth_room game nth] is the [nth] dungeon in [game] *)
val nth_room : t -> int -> Dungeon.t

(** [next_dungeon game dungeon] is the dungeon following [dungeon] in
    [game] *)
val next_dungeon : t -> Dungeon.t -> Dungeon.t

(** [prev_dungeon game dungeon] is the dungeon preceding [dungeon] in
    [game] *)
val prev_dungeon : t -> Dungeon.t -> Dungeon.t

(** [add_to_game game dungeon] is [game] with [dungeon] as the last
    dungon *)
val add_to_game : t -> Dungeon.t -> t

(** [game_depth game] is the number of dungeons in [game] *)
val game_depth : t -> int

(** [json_maker level hea liv e locx locy curid rnum cure e_b wn ar
    game] is a json save file that records current location [locx]
    [locy], current_id [curid], room number [rnum], current exp [cure],
    number of lives [liv], exp bound [e_b], weapon [wn], armor [ar], and
    [game], and boolean [e] of whether a saved game exists.*)
val json_maker :
  int ->
  int ->
  int ->
  bool ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  Item.t ->
  Item.t ->
  t ->
  Yojson.Basic.t

(** [update_file json] updates the current save file with [json] *)
val update_file : Yojson.Basic.t -> unit

(** [reset_save] is a blank save file *)
val reset_save : Yojson.Basic.t

(** [item_of_json json] is the item recorded in [json] *)
val item_of_json : Yojson.Basic.t -> Item.t
