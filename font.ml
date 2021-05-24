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
  if c = ' ' then "space"
  else if c = '?' then "question"
  else if c = '!' then "exclamation"
  else if c = '.' then "period"
  else c |> Char.lowercase_ascii |> Char.escaped

let render_char c x y char_width char_height =
  try
    Render.render_square
      (Render.new_square x y char_width char_height
         ("./fonts/" ^ char_to_file c ^ ".png"))
  with Not_found -> print_char c

let render_font ?(spacing = 0.1) bit =
  let w = bit.w and h = bit.h in
  String.iteri
    (fun i c ->
      render_char c
        ((foi i *. spacing) +. bit.x)
        bit.y (w *. 0.6) (h *. 0.6))
    bit.str
