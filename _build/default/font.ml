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
       ( "./fonts/"
       ^ (if int_of_char c = 32 then "space" else Char.escaped c)
       ^ ".png" ))

let render_font bit =
  let w = bit.w and h = bit.h in
  String.iteri
    (fun i c ->
      render_char c ((foi i *. 0.1) +. bit.x) bit.y (w *. 0.6) (h *. 0.6))
    bit.str
