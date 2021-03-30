type square = {
  mutable x : float;
  mutable y : float;
}

let renderAt ~x ~y ~rgb =
  GlMat.load_identity ();
  GlMat.translate3 (x, y, 0.);
  let r, g, b = rgb in
  GlDraw.color (r, g, b);
  GlDraw.begins `quads;
  List.iter GlDraw.vertex2
    [ (0., 0.); (0., 10.); (10., 10.); (10., 0.) ];
  GlDraw.ends ()

let render_square ~square = renderAt ~x:square.x ~y:square.y

let render_dungeon (dungeon : Dungeon.t) =
  let x_length = Dungeon.get_dimensions |> fst in
  let y_length = Dungeon.get_dimensions |> snd in
  Hashtbl.iter
    (fun (x, y) cell ->
      render_square
        {
          x = 1. /. float_of_int x_length *. float_of_int x *. 45;
          y = 1. /. float_of_int y_length *. float_of_int y *. 45;
        }
        ( if Dungeon.is_wall dungeon (i, j) then (0., 1., 0.)
        else (1., 1., 1.) ))
    dungeon.cells

let render_dungeon (dungeon : Dungeon.t) =
  for i = 0 to dungeon |> Dungeon.get_dimensions |> fst do
    for j = 0 to dungeon |> Dungeon.get_dimensions |> snd do
      render_square
        {
          x =
            1.
            /. float_of_int (dungeon |> Dungeon.get_dimensions |> fst)
            *. float_of_int i *. 2.;
          y =
            1.
            /. float_of_int (dungeon |> Dungeon.get_dimensions |> snd)
            *. float_of_int j *. 2.;
        }
        ( if Dungeon.is_wall dungeon (i, j) then (0., 1., 0.)
        else (1., 1., 1.) )
    done
  done

let move character = function
  | Glut.KEY_RIGHT ->
      character.x <- character.x +. 0.1;
      character
  | Glut.KEY_LEFT ->
      character.x <- character.x -. 0.1;
      character
  | _ -> character

let tile = ref { x = 1.; y = 1. }

let w = 500

and h = 500

let main () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w ~h;
  ignore (Glut.createWindow ~title:"fuck");
  Glut.displayFunc ~cb:(fun () ->
      GlClear.color (0.0, 0.0, 0.0);
      GlClear.clear [ `color ];
      GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
      GlMat.mode `projection;
      render_dungeon (Dungeon.instantiate_dungeon 10 10);
      Gl.flush ());
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key = 27 then exit 0);
  Glut.specialFunc ~cb:(fun ~key ~x ~y -> tile := move !tile key);
  (* Glut.idleFunc ~cb:(Some Glut.postRedisplay); *)
  Glut.mainLoop ()

let _ = main ()
