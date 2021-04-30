type direction =
  | Up
  | Down
  | Left
  | Right

type walker = {
  mutable current_pos : int * int;
  mutable dir : direction;
  mutable x_min : int;
  mutable x_max : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable step_history : (int * int) list;
  mutable steps_since_turn : int;
}

let directions = [ Up; Down; Left; Right ]

let init_walker xmin xmax ymin ymax =
  {
    current_pos = (1, 1);
    dir = Right;
    x_min = xmin;
    x_max = xmax;
    y_min = ymin;
    y_max = ymax;
    step_history = [ (1, 1) ];
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
    (target_x = w.x_max || target_x = w.x_min)
    || target_y = w.y_min || target_y = w.y_max
  then false
  else true

let step w =
  let target_pos = tup_add w.current_pos (eval_dir w.dir) in
  if determine_border w target_pos then (
    w.steps_since_turn <- w.steps_since_turn + 1;
    w.current_pos <- target_pos;
    true)
  else false

let rec choose_new_dir d =
  let dir = List.nth directions (Random.int 4) in
  if d = dir then choose_new_dir d else dir

let change_direction w =
  let rec helper w =
    let new_direction = choose_new_dir w.dir in
    let target_pos = tup_add w.current_pos (eval_dir new_direction) in
    if not (determine_border w target_pos) then new_direction
    else helper w
  in
  w.dir <- helper w

let walk steps w =
  let walk_helper () =
    if Random.float 1. >= 0.25 || w.steps_since_turn >= 4 then
      change_direction w
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
