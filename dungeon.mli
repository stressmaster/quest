type tile_sprite = string
type tile = {sprite : tile_sprite; is_wall : bool}
type dungeon = tile list list 