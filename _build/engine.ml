let init_engine textur_list w h x_length y_length =
  let dungeon = Dungeon.instantiate_dungeon 20 30 7 in
  let game = ref (State.init_state dungeon) in
  Bitmap.maximum_live := 15000000;
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r;
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
      let g = !game in
      render_dungeon (State.player_loc g) dungeon;
      Gl.flush ());
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key = 27 then exit 0);
  Glut.specialFunc ~cb:(fun ~key ~x ~y -> game := State.move !game key);
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.mainLoop ()
