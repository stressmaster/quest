let init_texture texture_list =
  Bitmap.maximum_live := 15000000;
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r;
  Texturemap.init_texture texture_list

let init_window w h =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w ~h;
  ignore (Glut.createWindow ~title:"CamelQuest")

let init_display dungeon game w h =
  Glut.displayFunc ~cb:(fun () ->
      GlClear.color (0.0, 0.0, 0.0);
      GlClear.clear [ `color ];
      GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
      GlMat.mode `projection;
      let g = !game in
      Dungeon.render_dungeon (State.player_loc g) dungeon;
      Gl.flush ())

let init_input game =
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key = 27 then exit 0);
  Glut.specialFunc ~cb:(fun ~key ~x ~y -> game := State.move !game key)

let init_engine texture_list w h x_length y_length =
  let game = ref (State.init_state "sample_game.json") in
  let curr_dungeon = State.curr_room !game in
  init_texture texture_list;
  init_window w h;
  init_display curr_dungeon game w h;
  init_input game;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.mainLoop ()
