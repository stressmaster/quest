open Images
open OImages
open Info

let textures = ref (Hashtbl.create 30)

(* [make_texture img] makes a texture out of [img]*)
let make_texture img =
  let w = img#width and h = img#height in
  let image = GlPix.create `ubyte ~format:`rgb ~width:w ~height:h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let pixel = img#get i j in
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
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d
    ( try Hashtbl.find !textures file
      with Not_found -> Hashtbl.find !textures "darkness.png" );
  List.iter
    (GlTex.parameter ~target:`texture_2d)
    [
      `wrap_s `clamp;
      `wrap_t `clamp;
      `mag_filter `nearest;
      `min_filter `nearest;
    ];
  GlTex.env (`mode `decal);
  start_texture ();
  GlDraw.shade_model `flat

(* [make_texture_list file_lst lst] is a list of textures made from
   files in [file_lst] *)
let make_texture_list file_lst lst =
  let add_to_textures h =
    Hashtbl.add lst h (make_texture (load_texture h)#to_rgb24)
  in
  List.iter add_to_textures file_lst;
  lst

(* [init_texture file_lst] is a list of filtered textures from
   [file_list] *)
let init_texture file_lst =
  textures := make_texture_list file_lst !textures
