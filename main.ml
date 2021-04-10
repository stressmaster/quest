let wall = "./wall.png"

and path = "./path.png"

and entrance = "./entrance.png"

and exit_tex = "./exit.png"

and player = "./player.png"

and darkness = "./darkness.png"

let texture_list = [ wall; path; entrance; exit_tex; player; darkness ]

let w = 500

and h = 500

let x_length = 11

let y_length = 11

let width = float_of_int w /. float_of_int x_length

let height = float_of_int h /. float_of_int y_length

let square_height ~height = float_of_int h /. height

(* [main] renders the game*)
let main () = Engine.init_engine texture_list w h x_length y_length

let _ = main ()
