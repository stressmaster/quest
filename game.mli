type t

val from_json : Yojson.Basic.t -> t

val start_room : t -> Dungeon.t

val next_dungeon : t -> Dungeon.t -> Dungeon.t

val prev_dungeon : t -> Dungeon.t -> Dungeon.t

val add_to_game : t -> Dungeon.t -> t

val game_depth : t -> int

val json_maker : bool -> int -> t -> Yojson.Basic.t

val update_file : Yojson.Basic.t -> unit
