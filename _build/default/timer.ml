let time = ref 0

let current_time () = !time

let reset_timer () = time := 0

let increase_time t = time := !time + t
