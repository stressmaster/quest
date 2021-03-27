open GtkMain
open GdkKeysyms
open Dungeon

let locale = GtkMain.Main.init ()

let renderWall () =
  GlDraw.color (0., 0., 0.);
  GlDraw.begins `quads;
  List.iter GlDraw.vertex2
    [ (-10., -10.); (-10., 10.); (10., 10.); (10., -10.) ];
  GlDraw.ends ()

let renderPath () =
  GlDraw.color (1., 0., 1.);
  GlDraw.begins `quads;
  List.iter GlDraw.vertex2
    [ (-10., -10.); (-10., 10.); (10., 10.); (10., -10.) ];
  GlDraw.ends ()

(*GlMat.translate3(, invader.y, 0.0);*)

let render (dungeon : Dungeon.t) =
  GlMat.load_identity ();
  for y = 0 to dungeon |> get_dimensions |> fst do
    for x = 0 to dungeon |> get_dimensions |> snd do
      GlMat.translate3 (float_of_int x, float_of_int x, 0.0);
      if is_wall dungeon (x, y) then renderWall () else renderPath ()
    done
  done

let main () =
  let window =
    GWindow.window ~width:640 ~height:480 ~title:"CamelQuest" ()
  in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

  (* Button let button = GButton.button ~label:"Push me!"
     ~packing:vbox#add () in button#connect#clicked ~callback:(fun () ->
     prerr_endline "Ouch!"); *)
  render (instantiate_dungeon 10 10);

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
