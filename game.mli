type t

type monster

val monster_of_json : Yojson.Basic.t -> monster

val dungeon_of_monster : Yojson.Basic.t -> Dungeon.t

val from_json : Yojson.Basic.t -> t
