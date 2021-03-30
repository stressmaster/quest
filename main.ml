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

let render_dungeon ~n ~m =
  for i = 0 to n do
    for j = 0 to m do
      render_square
        {
          x = 1. /. float_of_int n *. float_of_int i *. 2.;
          y = 1. /. float_of_int m *. float_of_int j *. 2.;
        }
        (if i = j then (0., 1., 0.) else (1., 1., 1.))
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
      render_dungeon ~n:(w / 10) ~m:(h / 10);
      Gl.flush ());
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key = 27 then exit 0);
  Glut.specialFunc ~cb:(fun ~key ~x ~y -> tile := move !tile key);
  (* Glut.idleFunc ~cb:(Some Glut.postRedisplay); *)
  Glut.mainLoop ()

let _ = main ()
