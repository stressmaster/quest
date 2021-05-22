(* an abstract type representing a dungeon*)
type t

(* an abstract type representing a tile*)
type tile

(* an abstract type representing a dungeon cell*)
type cell

(* [instantiate_dungeon x y] is a dungeon with [x] columns [y] rows *)
val instantiate_dungeon :
  int -> int -> int -> int * int -> int -> int -> int -> t

val instantiate_dungeon_with_seed :
  int -> int -> int -> int * int -> int -> int -> int -> int -> t

(* [is_wall dungeon (x, y)] returns true iff the cell at coordinate [(x,
   y)] is a wall in [dungeon] *)
val is_wall : t -> int * int -> bool

val get_item : t -> int * int -> Item.t option

val drop_item : t -> int * int -> Item.t option -> unit

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

val get_time : t -> int

val render_dungeon : int * int -> t -> bool -> unit

val get_magic_numbers : t -> Magic_numbers.t
