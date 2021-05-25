(** This module contains the data necessary to manage mutable textures*)

(** [init_texture files] initializes texture with [files]*)
val init_texture : string list -> unit

(** [set_texture file] sets texture to texture of [file]*)
val set_texture : string -> unit

(** [start_texture ()] enables texture *)
val start_texture : unit -> unit

(** [end_texture ()] disables texture *)
val end_texture : unit -> unit
