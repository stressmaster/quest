(** Represent animations

    This module makes animations. It includes functions for initiating
    animations and getting the current frame *)

(** abstract type representing an animation *)
type animation

(** type representing the id of a sprite *)
type id = string

(** [init_animations animations] initiates [animations] *)
val init_animations : (id * string list) list -> unit

(** [get_sprite id] is the current frame of the sprite with id [id] *)
val get_sprite : id -> string

(** [step_animation ()] steps the frames of all animations *)
val step_animation : unit -> unit
