(* main *)
open GtkMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let main () =
  let window =
    GWindow.window ~width:320 ~height:240
      ~title:"Simple lablgtk program" ()
  in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit
