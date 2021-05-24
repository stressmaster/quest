type direction =
  | Up
  | Down
  | Left
  | Right

type walker = {
  mutable furthest_pos : int * int;
  mutable start_pos : int * int;
  mutable current_pos : int * int;
  mutable dir : direction;
  mutable x_min : int;
  mutable x_max : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable step_history : (int * int) list;
  mutable steps_since_turn : int;
}

let init_walker xmin xmax ymin ymax start =
  {
    furthest_pos = start;
    start_pos = start;
    current_pos = start;
    dir = Right;
    x_min = xmin;
    x_max = xmax;
    y_min = ymin;
    y_max = ymax;
    step_history = [ start ];
    steps_since_turn = 0;
  }

let eval_dir d =
  match d with
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let tup_add t1 t2 = (fst t1 + fst t2, snd t1 + snd t2)

let determine_border w target =
  let target_x = fst target in
  let target_y = snd target in
  if
    (target_x >= w.x_max - 1 || target_x <= w.x_min)
    || target_y <= w.y_min
    || target_y >= w.y_max - 1
  then false
  else true

let step w =
  let target_pos = tup_add w.current_pos (eval_dir w.dir) in
  let current = w.furthest_pos in
  if determine_border w target_pos then (
    w.furthest_pos <-
      (let newxdist = fst target_pos - fst w.start_pos in
       let newydist = snd target_pos - snd w.start_pos in
       let oldxdist = fst current - fst w.start_pos in
       let oldydist = snd current - snd w.start_pos in
       if
         (newxdist * newxdist) + (newydist * newydist)
         > (oldxdist * oldxdist) + (oldydist * oldydist)
       then target_pos
       else current);
    w.steps_since_turn <- w.steps_since_turn + 1;
    w.current_pos <- target_pos;
    true)
  else false

let debug_dir d =
  match d with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let change_direction w =
  (* print_string ("changing dir: " ^ debug_dir w.dir); print_newline
     (); *)
  let rand = Random.bool () in
  if w.dir = Up || w.dir = Down then
    match rand with true -> w.dir <- Left | false -> w.dir <- Right
  else match rand with true -> w.dir <- Up | false -> w.dir <- Down

let walk steps w =
  let walk_helper () =
    if Random.float 1. >= 0.4 || w.steps_since_turn > 8 then (
      w.steps_since_turn <- 0;
      change_direction w)
    else if w.steps_since_turn > 8 then (
      w.steps_since_turn <- 0;
      change_direction w)
    else if step w then
      w.step_history <- w.current_pos :: w.step_history
    else change_direction w
  in
  let rec times index =
    walk_helper ();
    if index = 0 then () else times (index - 1)
  in
  times steps;
  w.step_history
