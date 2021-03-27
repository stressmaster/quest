type tile_sprite = string

type color = | Green | Gray

type material = 
| Color of color
| Sprite of tile_sprite

type tile = {sprite : tile_sprite; material : material; is_wall : bool}

type cell = {tile: tile; x : int; y : int}

type dungeon = {
  cells : (int * int, cell) Hashtbl.t;
  start : (int * int); 
  dimensions : (int * int);
}

val instantiate_dungeon : int -> int -> dungeon
