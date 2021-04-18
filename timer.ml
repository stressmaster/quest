let start_time = ref (Sys.time ())

let current_time () = !start_time -. Sys.time ()

let reset_timer () = start_time := Sys.time ()
