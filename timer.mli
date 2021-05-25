(**Representation of timer data.

   This module contains the mutable data used to keep track of time
   elapsed within the game.*)

(** The strings by which we identify each timer*)
type id = string

(** The abstract type reepresenting time*)
type time

(** [init_timers \[(id1, i1), (id2, i2), ... (idk, ik)\]] initializes
    timers [id1 ... idk] with corresponding increments [i1 ... ik]. Each
    timer will then increment by that amount each time in step_timer (). *)
val init_timers : (id * int) list -> unit

(** [current_time id] shows the current amount of time recorded by timer
    [id]. *)
val current_time : id -> int

(** [reset_timer id] resets the timer [id]*)
val reset_timer : id -> unit

(** [step_timer ()] steps each one of the timers according to their
    increment value *)
val step_timer : unit -> unit
