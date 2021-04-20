let start_time = ref (Sys.time ())

let reset_timer () = start_time := Sys.time ()

let current_time_raw () = !start_time -. Sys.time ()

let delta_time =
  reset_timer ();
  let rec range a =
    if current_time_raw () <= -0.005 then a else range (a +. 1.)
  in
  0.0001 *. range 1.

let current_time () = (!start_time -. Sys.time ()) /. delta_time
