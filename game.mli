type t

type monster

val from_json : Yojson.Basic.t -> t

val start_room : t -> Dungeon.t
