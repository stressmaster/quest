(** This module contains the data necessary to manage mutable textures*)

(** [init_texture] initializes texture *)
val init_texture : string list -> unit

(** [set_texture] sets texture *)
val set_texture : string -> unit

(** [start_texture] enables texture *)
val start_texture : unit -> unit

(** [end_texture] disables texture *)
val end_texture : unit -> unit
