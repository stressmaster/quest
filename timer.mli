type id = string

type time

val init_timers : (id * int) list -> unit

(* [current_time] shows the current distance from start *)
val current_time : id -> int

(* [reset_timer] resets the timer *)
val reset_timer : id -> unit

(* [step_timer] steps each one of the timer *)
val step_timer : unit -> unit
