type bitmap_string = {
  str : string;
  x : float;
  y : float;
  w : float;
  h : float;
}

let foi i = float_of_int i

let new_font str x y w h = { str; x; y; w; h }

let render_char c x y char_width char_height =
  Render.render_square
    (Render.new_square x y char_width char_height
       ("./font" ^ Char.escaped c ^ ".png"))

let render_font bit =
  let w = bit.w and h = bit.h in
  String.iteri
    (fun i c ->
      render_char c ((foi i *. w) +. bit.x) ((foi i *. h) +. bit.y) w h)
    bit.str
