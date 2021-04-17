open Images
open OImages
open Info

let texture_list = ref []

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

let get_texture file = List.assoc file !texture_list

let set_texture texture =
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d texture;
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
let rec make_texture_list file_lst lst =
  match file_lst with
  | [] -> lst
  | h :: t ->
      make_texture_list t
        ((h, make_texture (load_texture h)#to_rgb24) :: lst)

(* [init_texture file_lst] is a list of filtered textures from
   [file_list] *)
let init_texture file_lst =
  texture_list := make_texture_list file_lst !texture_list
