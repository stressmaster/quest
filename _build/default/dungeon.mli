(* an abstract type representing a dungeon*)
type t

(* an abstract type representing a tile*)
type tile

(* an abstract type representing a dungeon cell*)
type cell

type tile_sprite = string

type material = Sprite of tile_sprite

(* [instantiate_dungeon x y] is a dungeon with [x] columns [y] rows *)
val instantiate_dungeon : int -> int -> t

(* [is_wall dungeon (x, y)] returns true iff the cell at coordinate [(x,
   y)] is a wall in [dungeon] *)
val is_wall : t -> int * int -> bool

(* [is_wall dungeon (x, y)] is the material of the tile at [(x,y)] in
   [dungeon] *)
val tile_material : tile -> material

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
