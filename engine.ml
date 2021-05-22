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

let dungeon_render_helper game b =
  Dungeon.render_dungeon
    (State.player_loc !game)
    (State.curr_room !game) b

let fight_render_helper game =
  Fight_menu.render_menu (State.curr_fight !game)

let spiral_render_helper game =
  Spiral.render_spiral
    (State.curr_fight !game)
    !Magic_numbers.get_magic.x_length !Magic_numbers.get_magic.y_length

let what_to_render game = function
  | Render_stack.SpiralRender ->
      dungeon_render_helper game false;
      spiral_render_helper game
  | Render_stack.AttackRender ->
      fight_render_helper game;
      Fight_menu.render_attack (State.curr_fight !game)
  | Render_stack.ScreenshakeRender ->
      fight_render_helper game;
      Fight_menu.render_player_damage ()
  | Render_stack.FightRender ->
      Fight_menu.render_menu (State.curr_fight !game)
  | Render_stack.DungeonRender ->
      dungeon_render_helper game true;
      State.render_inventory !game
  | Render_stack.GameoverRender ->
      Gameover_menu.render_menu (State.curr_game_over !game)
  | _ -> ()

let init_display game w h =
  let cb () =
    GlClear.color (0.0, 0.0, 0.0);
    GlClear.clear [ `color ];
    GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
    GlMat.mode `projection;
    what_to_render game (Render_stack.stack_peek ());
    Gl.flush ()
  in
  Glut.displayFunc ~cb

let init_input game =
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y ->
      if key = 27 then exit 0 else game := State.typing_move !game key);
  Glut.specialFunc ~cb:(fun ~key ~x ~y ->
      game := State.controller !game key)

let rec bigtimer ~value =
  Bigtimer.increase_time 1;
  Glut.postRedisplay ();
  Glut.timerFunc ~ms:value ~cb:bigtimer ~value

let rec general_timer ~value =
  Timer.increase_time 1;
  Glut.postRedisplay ();
  Glut.timerFunc ~ms:value ~cb:general_timer ~value

let rec animation_timer ~value =
  Spriteanimation.step_animation ();
  Glut.timerFunc ~ms:value ~cb:animation_timer ~value

let init_timer ms game =
  let rec typing_timer ~value =
    game := State.check_time_limit !game;
    Glut.timerFunc ~ms:value ~cb:typing_timer ~value
  in
  Glut.timerFunc ~ms ~cb:bigtimer ~value:ms;
  Glut.timerFunc ~ms ~cb:general_timer ~value:ms;
  Glut.timerFunc ~ms ~cb:typing_timer ~value:ms;
  Glut.timerFunc ~ms ~cb:animation_timer ~value:ms

let init_engine texture_list w h x_length y_length =
  (* check exists. if exists, then State.init_state_from_json
     "save.json"*)
  (* let exists = Yojson.Basic.from_file "save.json" |>
     Yojson.Basic.Util.member "exists" in *)
  let game = ref (State.init_state "sample_game.json") in
  let start = Sys.time () in
  init_texture texture_list;
  init_audio ();
  init_window w h;
  init_animation ();
  init_display game w h;
  init_input game;
  let ms = 100. *. (Sys.time () -. start) |> int_of_float in
  init_timer ms game;
  (*Glut.idleFunc ~cb:(Some Glut.postRedisplay);*)
  Glut.mainLoop ()
