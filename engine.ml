let init_texture texture_list = Texturemap.init_texture texture_list

let init_audio () = Audio.play_music ()

let init_animation () =
  Spriteanimation.init_animations !Magic_numbers.get_magic.animations

let stack = Stack.create ()

let _ = Stack.push Render_stack.DungeonRender stack

let stack_push x = Stack.push x stack

let stack_pop () = Stack.pop stack

let init_window w h =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w ~h;
  ignore (Glut.createWindow ~title:"CamelQuest")

let init_display game w h =
  Glut.displayFunc ~cb:(fun () ->
      GlClear.color (0.0, 0.0, 0.0);
      GlClear.clear [ `color ];
      GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
      GlMat.mode `projection;
      ( match Render_stack.stack_peek () with
      | SpiralRender ->
          Dungeon.render_dungeon
            (State.player_loc !game)
            (State.curr_room !game) false;
          Spiral.render_spiral
            (State.curr_fight !game)
            !Magic_numbers.get_magic.x_length
            !Magic_numbers.get_magic.y_length
      | AttackRender ->
          Fight_menu.render_menu (State.curr_fight !game);
          Fight_menu.render_attack (State.curr_fight !game)
      | ScreenshakeRender ->
          Fight_menu.render_menu (State.curr_fight !game);
          Fight_menu.render_player_damage ()
      | FightRender -> Fight_menu.render_menu (State.curr_fight !game)
      | DungeonRender ->
          Dungeon.render_dungeon
            (State.player_loc !game)
            (State.curr_room !game) true;
          State.render_inventory !game
      | _ -> () );

      Gl.flush ()
      (* (Font.render_font (Font.new_font (string_of_int
         (Timer.current_time ())) 1.5 1.8
         (!Magic_numbers.get_magic).width
         (!(!Magic_numbers.get_magic).get_magic).height)) *))

let init_input game =
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y ->
      if key = 27 then exit 0 else game := State.typing_move !game key);
  Glut.specialFunc ~cb:(fun ~key ~x ~y ->
      game := State.controller !game key)

let init_engine texture_list w h x_length y_length =
  let game = ref (State.init_state "sample_game.json") in
  let start = Sys.time () in
  init_texture texture_list;
  init_audio ();
  init_window w h;
  init_animation ();
  init_display game w h;
  init_input game;
  let rec bigtimer ~value =
    Bigtimer.increase_time 1;
    Glut.postRedisplay ();
    Glut.timerFunc ~ms:value ~cb:bigtimer ~value
  in
  let rec timer ~value =
    Timer.increase_time 1;
    Glut.postRedisplay ();
    Glut.timerFunc ~ms:value ~cb:timer ~value
  in
  let rec typing_timer ~value =
    game := State.check_time_limit !game;
    Glut.timerFunc ~ms:value ~cb:typing_timer ~value
  in
  let rec animation_timer ~value =
    Spriteanimation.step_animation ();
    Glut.timerFunc ~ms:value ~cb:animation_timer ~value
  in
  let ms = 200. *. (Sys.time () -. start) |> int_of_float in
  (*Glut.idleFunc ~cb:(Some Glut.postRedisplay);*)
  Glut.timerFunc ~ms ~cb:bigtimer ~value:ms;
  Glut.timerFunc ~ms ~cb:timer ~value:ms;
  Glut.timerFunc ~ms ~cb:typing_timer ~value:ms;
  Glut.timerFunc ~ms ~cb:animation_timer ~value:ms;
  Glut.mainLoop ()
