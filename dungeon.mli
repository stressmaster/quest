(** Representation of the dungeon.

    This module represents the data needed to maintain and render the
    dungeon, including a data about each tile. Includes methods for
    obtaining information about the dungeon. *)

(** an abstract type representing a dungeon*)
type t

(** an abstract type representing a tile *)
type tile

(** an abstract type representing a dungeon cell *)
type cell

(** [instantiate_dungeon ?seed id x y start bound next prev] is a
    dungeon with [x] columns [y] rows, id [id], start position at
    [start], monster encounter rate [bound], and optionally with [?seed]*)
val instantiate_dungeon :
  ?seed:int -> int -> int -> int -> int * int -> int -> int -> int -> t

(** [is_wall dungeon (x, y)] is true iff the cell at coordinate [(x, y)]
    is a wall in [dungeon] *)
val is_wall : t -> int * int -> bool

(** [get_item dungeon (x,y)] returns the Item.t option associated with
    the tile at [(x,y)] in [dungeon], and then removes the item from
    that tile *)
val get_item : t -> int * int -> Item.t option

(**[drop_item dungeon (x,y) item] places [item] on the tile at [(x,y)] *)
val drop_item : t -> int * int -> Item.t option -> unit

(** [tile_material tile] is the material of [tile] *)
val tile_material : tile -> string

(** [get_start dungeon] is the coordinates of the starting tile in
    [dungeon] *)
val get_start : t -> int * int

(** [get_exit dungeon] is the coordinates of the exit tile in [dungeon] *)
val get_exit : t -> int * int

(** [get_dimensions dungeon] is the pair of ints representing the
    dimensions of [dungeon]*)
val get_dimensions : t -> int * int

(** [get_cells dungeon] is the hash table representing the cells in
    [dungeon]*)
val get_cells : t -> (int * int, cell) Hashtbl.t

(** [get_tile cell] is the tile associated with [cell] *)
val get_tile : cell -> tile

(** [get_bound dungeon] is the bound of [dungeon]*)
val get_bound : t -> int

(** [get_id dungeon] is the id of [dungeon]*)
val get_id : t -> int

(** [get_next dungeon] is the id of the dungeon that follows [dungeon]*)
val get_next : t -> int

(** [get_prev dungeon] is the id of the dungeon that precedes [dungeon]*)
val get_prev : t -> int

(** [get_time dungeon] is the time seed associated with [dungeon] *)
val get_time : t -> int

(** [render_dungeon (x,y) dungeon condition] renders [dungeon] with
    dungeon coordinates [(x,y)] at the center of the window and with
    NPCs if [condition] *)
val render_dungeon : int * int -> t -> bool -> unit

(** [get_magic_numbers dungeon] is the magic_numbers associated with
    [dungeon] *)
val get_magic_numbers : t -> Magic_numbers.t
