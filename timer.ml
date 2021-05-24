type id = string

type time = {
  increment : int;
  ticker : int ref;
}

let timers = ref []

let init_timers (id_increment_list : (id * int) list) =
  let add_to_timers h =
    timers := (fst h, { increment = snd h; ticker = ref 0 }) :: !timers
  in
  List.iter add_to_timers id_increment_list

let current_time id =
  let { increment; ticker } = List.assoc id !timers in
  !ticker

let reset_timer id =
  let { increment; ticker } = List.assoc id !timers in
  ticker := 0

let step_timer () =
  let step_ticker h =
    let id, { increment; ticker } = h in
    ticker := (!ticker + increment) mod max_int;
    h
  in
  timers := List.map step_ticker !timers
