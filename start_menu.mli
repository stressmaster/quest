(** This module renders the menu which appears upon first opening the
    game*)

(** [render_menu action] renders the menu, and draws an arrow based on
    [action]*)
val render_menu : State.start_menu_action -> unit
