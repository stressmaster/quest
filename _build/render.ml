type square = {
  mutable square_x : float;
  mutable square_y : float;
  mutable width : float;
  mutable height : float;
  mutable texture : string;
}

let new_square x y w h t =
  { square_x = x; square_y = y; width = w; height = h; texture = t }

let renderAt ~x ~y ~width ~height ~texture =
  GlMat.load_identity ();
  GlMat.translate3 (x, y, 0.);
  Texturemap.set_texture texture;
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
  renderAt ~x:square.square_x ~y:square.square_y ~width:square.width
    ~height:square.height ~texture:square.texture
