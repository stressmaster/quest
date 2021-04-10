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

(*let dungeon = Dungeon.instantiate_dungeon 20 30 7

  let game = ref (State.init_state dungeon)*)

let _ =
  Bitmap.maximum_live := 15000000;
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r

(* [main] renders the game*)
let main () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w ~h;
  ignore (Glut.createWindow ~title:"CamelQuest");
  Texturemap.init_texture texture_list;
  Glut.displayFunc ~cb:(fun () ->
      GlClear.color (0.0, 0.0, 0.0);
      GlClear.clear [ `color ];
      GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
      GlMat.mode `projection;
      (*let g = !game in*)
      (*Dungeon.render_dungeon (State.player_loc g) dungeon;*)
      Gl.flush ());
  (*Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key = 27 then exit 0);
    Glut.specialFunc ~cb:(fun ~key ~x ~y -> game := State.move !game
    key); Glut.idleFunc ~cb:(Some Glut.postRedisplay);*)
  Glut.mainLoop ()

let _ = main ()
