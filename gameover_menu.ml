let render_menu action =
  Font.render_font
    (Font.new_font "you" 0.75 1. !Magic_numbers.get_magic.width
       !Magic_numbers.get_magic.height);
  Font.render_font
    (Font.new_font "died!" 0.75 0.9 !Magic_numbers.get_magic.width
       !Magic_numbers.get_magic.height);
  Font.render_font
    (Font.new_font "quit" 0.4 0.6 !Magic_numbers.get_magic.width
       !Magic_numbers.get_magic.height);
  Font.render_font
    (Font.new_font "revive" 0.4 0.4 !Magic_numbers.get_magic.width
       !Magic_numbers.get_magic.height);
  Font.render_font
    (Font.new_font "restart" 0.4 0.2 !Magic_numbers.get_magic.width
       !Magic_numbers.get_magic.height);
  match action with
  | State.Quit ->
      Font.render_font
        (Font.new_font ">" 0.3 0.6 !Magic_numbers.get_magic.width
           !Magic_numbers.get_magic.height)
  | State.Revive ->
      Font.render_font
        (Font.new_font ">" 0.3 0.4 !Magic_numbers.get_magic.width
           !Magic_numbers.get_magic.height)
  | State.Restart ->
      Font.render_font
        (Font.new_font ">" 0.3 0.2 !Magic_numbers.get_magic.width
           !Magic_numbers.get_magic.height)
