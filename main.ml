(* main *)
open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()
let main () =
  let window = GWindow.window ~width:320 ~height:240 ~title:"Fuck" ()