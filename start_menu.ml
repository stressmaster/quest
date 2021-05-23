let render_menu action =
  Font.render_font ~spacing:0.2
    (Font.new_font "camelquest" 0. 1.
       (2. *. Magic_numbers.width)
       (2. *. Magic_numbers.height));
  Font.render_font
    (Font.new_font "new game" 0.4 0.4 Magic_numbers.width
       Magic_numbers.height);
  Font.render_font
    (Font.new_font "continue" 0.4 0.2 Magic_numbers.width
       Magic_numbers.height);
  match action with
  | State.NewGame ->
      Font.render_font
        (Font.new_font ">" 0.3 0.4 Magic_numbers.width
           Magic_numbers.height)
  | State.Continue ->
      Font.render_font
        (Font.new_font ">" 0.3 0.2 Magic_numbers.width
           Magic_numbers.height)
