let image_height = 50

and image_width = 50

let make_image () =
  let image =
    GlPix.create `ubyte ~format:`rgb ~width:image_width
      ~height:image_height
  in
  for i = 0 to image_width - 1 do
    for j = 0 to image_height - 1 do
      Raw.sets (GlPix.to_raw image)
        ~pos:(3 * ((i * image_height) + j))
        ( if i land 8 lxor (j land 8) = 0 then [| 255; 255; 255 |]
        else [| 0; 0; 0 |] )
    done
  done;
  image

let init_texture () =
  let image = make_image () in
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d image;
  List.iter
    (GlTex.parameter ~target:`texture_2d)
    [
      `wrap_s `clamp;
      `wrap_t `clamp;
      `mag_filter `nearest;
      `min_filter `nearest;
    ];
  GlTex.env (`mode `decal);
  Gl.enable `texture_2d
