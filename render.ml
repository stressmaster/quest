type square = {
  mutable x : float;
  mutable y : float;
  mutable width : float;
  mutable height : float;
  mutable texture : string;
}

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
