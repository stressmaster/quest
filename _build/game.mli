type t

type monster

val from_json : Yojson.Basic.t -> t

val start_room : t -> Dungeon.t

val next_dungeon : t -> Dungeon.t -> Dungeon.t

val prev_dungeon : t -> Dungeon.t -> Dungeon.t
