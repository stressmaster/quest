let image_height = 40

and image_width = 40

let make_texture color1 color2 =
  let image =
    GlPix.create `ubyte ~format:`rgb ~width:image_width
      ~height:image_height
  in
  for i = 0 to image_width - 1 do
    for j = 0 to image_height - 1 do
      Raw.sets (GlPix.to_raw image)
        ~pos:(3 * ((i * image_height) + j))
        (if i land 8 lxor (j land 8) = 0 then color1 else color2)
    done
  done;
  image

let start_texture () = Gl.enable `texture_2d

let end_texture () = Gl.disable `texture_2d

let set_texture color1 color2 =
  GlTex.image2d (make_texture color1 color2)

let init_texture () =
  GlPix.store (`unpack_alignment 1);
  List.iter
    (GlTex.parameter ~target:`texture_2d)
    [
      `wrap_s `clamp;
      `wrap_t `clamp;
      `mag_filter `nearest;
      `min_filter `nearest;
    ];
  GlTex.env (`mode `decal);
  ()
