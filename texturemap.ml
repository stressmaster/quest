open Images
open OImages
open Info

let make_texture img =
  let w = img#width and h = img#height in
  let image = GlPix.create `ubyte ~format:`rgb ~width:w ~height:h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let pixel = img#get i j in
      (* pixel is a Color.rgb *)
      Raw.sets (GlPix.to_raw image)
        ~pos:(3 * ((i * h) + j))
        [| pixel.r; pixel.g; pixel.b |]
    done
  done;
  image

let start_texture () = Gl.enable `texture_2d

let end_texture () = Gl.disable `texture_2d

let load_texture file = OImages.load file [] |> OImages.rgba32

let set_texture file =
  let img = load_texture file in
  GlTex.image2d (make_texture img#to_rgb24)

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
