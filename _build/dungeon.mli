(* an abstract type representing a dungeon*)
type t

type monster = {
  name : string;
  sprite : string;
  mutable hitpoints : int;
  encounter_chance : int;
  attack_strings : string list;
  max_hp : int;
}

(* an abstract type representing a tile*)
type tile

(* an abstract type representing a dungeon cell*)
type cell

val instantiate_monster :
  string -> string -> int -> int -> string list -> monster

(* [instantiate_dungeon x y] is a dungeon with [x] columns [y] rows *)
val instantiate_dungeon :
  int ->
  int ->
  int ->
  int * int ->
  int ->
  monster list ->
  int ->
  int ->
  t

(* [is_wall dungeon (x, y)] returns true iff the cell at coordinate [(x,
   y)] is a wall in [dungeon] *)
val is_wall : t -> int * int -> bool

val get_item : t -> int * int -> Item.t option

val get_monster : t -> monster

val get_monsters : t -> monster list

(* [is_wall dungeon (x, y)] is the material of the tile at [(x,y)] in
   [dungeon] *)
val tile_material : tile -> string

(* [get_start dungeon] is the coordinates of the starting tile in
   [dungeon]*)
val get_start : t -> int * int

(* [get_exit dungeon] is the coordinates of the exit tile in [dungeon]*)
val get_exit : t -> int * int

(* [get_dimensions dungeon] is the pair of ints representing the
   dimensions of [dungeon]*)
val get_dimensions : t -> int * int

(* [get_cells dungeon] is the hash table representing the cells in
   [dungeon]*)
val get_cells : t -> (int * int, cell) Hashtbl.t

(* [get_cells cell] is the tile of [cell]*)
val get_tile : cell -> tile

(* [get_bound dungeon] is the current bound of [dungeon]*)
val get_bound : t -> int

val get_id : t -> int

val get_next : t -> int

val get_prev : t -> int

val get_monster_string : monster -> string

val get_monster_HP : monster -> int

val get_monster_max_HP : monster -> int

val render_dungeon : int * int -> t -> bool -> unit

val monster_move : monster -> string
