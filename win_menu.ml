let render_menu () =
  Render.render_square
    (Render.new_square 0. 0. 500. 500. "./clarkson.png");
  Font.render_font
    (Font.new_font "Congratulations!" 0.2 0. Magic_numbers.width
       Magic_numbers.height)
