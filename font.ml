type bitmap_string = {
  str : string;
  x : float;
  y : float;
  w : float;
  h : float;
}

let foi i = float_of_int i

let new_font str x y w h = { str; x; y; w; h }

let char_to_file c =
  if int_of_char c = 32 then "space"
  else c |> Char.lowercase_ascii |> Char.escaped

let render_char c x y char_width char_height =
  try
    Render.render_square
      (Render.new_square x y char_width char_height
         ("./fonts/" ^ char_to_file c ^ ".png"))
  with Not_found -> print_int (int_of_char c)

let render_font bit =
  let w = bit.w and h = bit.h in
  String.iteri
    (fun i c ->
      render_char c ((foi i *. 0.1) +. bit.x) bit.y (w *. 0.6) (h *. 0.6))
    bit.str
