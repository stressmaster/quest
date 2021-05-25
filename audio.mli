(** Represeents the audio of the game.

    This module plays music and includes functions for changing music. *)

(** abstract type representing audio *)
type t

(** [set_music music] initializes the music to [music] *)
val set_music : string -> unit

(** [play_music ()] plays the current music instance *)
val play_music : unit -> unit

(** [halt_music ()] halts the current music instance *)
val halt_music : unit -> unit

(** [change_music music] changes the music to [music] *)
val change_music : string -> unit

(** [play_sound sound] plays [sound] *)
val play_sound : string -> unit
