(* [init_texture] initializes texture *)
val init_texture : string list -> unit

(* [get_texture texture_name] returns texture*)
val get_texture : string -> 'a

(* [set_texture] sets texture *)
val set_texture : 'a -> unit

(* [start_texture] enables texture *)
val start_texture : unit -> unit

(* [end_texture] disables texture*)
val end_texture : unit -> unit
