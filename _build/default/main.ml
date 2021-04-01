type square = {
  mutable x : float;
  mutable y : float;
  mutable width : float;
  mutable height : float;
  mutable texture : string;
}

let wall = "wall.png"

and path = "path.png"

and entrance = "entrance.png"

and exit_tex = "exit.png"

and player = "player.png"

and darkness = "darkness.png"

let texture_list = [ wall; path; entrance; exit_tex; player; darkness ]

let w = 500

and h = 500

let x_length = 11

let y_length = 11

let width = float_of_int w /. float_of_int x_length

let height = float_of_int h /. float_of_int y_length

let square_height ~height = float_of_int h /. height

let renderAt ~x ~y ~width ~height ~texture =
  GlMat.load_identity ();
  GlMat.translate3 (x, y, 0.);
  Texturemap.set_texture texture;
  Texturemap.start_texture ();
  GlDraw.begins `quads;
  GlTex.coord2 (0.0, 0.0);
  GlDraw.vertex2 (0., 0.);
  GlTex.coord2 (0.0, 1.0);
  GlDraw.vertex2 (0., height -. 1.);
  GlTex.coord2 (1.0, 1.0);
  GlDraw.vertex2 (width -. 1., height -. 1.);
  GlTex.coord2 (1.0, 0.0);
  GlDraw.vertex2 (width -. 1., 0.);
  Texturemap.end_texture ();
  GlDraw.ends ()

let render_square ~square =
  renderAt ~x:square.x ~y:square.y ~width:square.width
    ~height:square.height ~texture:square.texture

let determine_color tile =
  let material = tile |> Dungeon.tile_material in
  match material with
  | Sprite s ->
      if s = "wall.jpg" then wall
      else if s = "path.jpg" then path
      else darkness

let render_dungeon (p_x, p_y) (dungeon : Dungeon.t) =
  let dungeon_cells = dungeon |> Dungeon.get_cells in
  let dungeon_x_length = dungeon |> Dungeon.get_dimensions |> fst in
  let dungeon_y_length = dungeon |> Dungeon.get_dimensions |> snd in
  let x_start =
    if p_x - ((x_length - 1) / 2) < 0 then 0
    else if p_x + ((x_length - 1) / 2) + 1 > dungeon_x_length then
      dungeon_x_length - x_length
    else p_x - ((x_length - 1) / 2)
  in
  let x_end =
    if p_x + ((x_length - 1) / 2) + 1 > dungeon_x_length then
      dungeon_x_length
    else if p_x - ((x_length - 1) / 2) < 0 then x_length
    else p_x + ((x_length - 1) / 2)
  in
  let y_start =
    if p_y - ((y_length - 1) / 2) < 0 then 0
    else if p_y + ((y_length - 1) / 2) + 1 > dungeon_y_length then
      dungeon_y_length - y_length
    else p_y - ((y_length - 1) / 2)
  in
  let y_end =
    if p_y + ((y_length - 1) / 2) + 1 > dungeon_y_length then
      dungeon_y_length
    else if p_y - ((y_length - 1) / 2) < 0 then y_length
    else p_y + ((y_length - 1) / 2)
  in
  for x = x_start to x_end do
    for y = y_start to y_end do
      let texture =
        if (x, y) = (p_x, p_y) then player
        else if Hashtbl.find_opt dungeon_cells (x, y) = None then
          darkness
        else if (x, y) = Dungeon.get_start dungeon then entrance
        else if Dungeon.get_exit dungeon = (x, y) then exit_tex
        else
          determine_color
            (Hashtbl.find dungeon_cells (x, y) |> Dungeon.get_tile)
      in
      render_square
        {
          x = float_of_int (x - x_start) /. float_of_int x_length *. 2.;
          y = float_of_int (y - y_start) /. float_of_int y_length *. 2.;
          width;
          height;
          texture;
        }
    done
  done

let dungeon = Dungeon.instantiate_dungeon 20 30

let game = ref (State.init_state dungeon)

let _ =
  Bitmap.maximum_live := 15000000;
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r

let main () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w ~h;
  ignore (Glut.createWindow ~title:"fuck");
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

let _ = main ()
