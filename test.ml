open OUnit2
open Dungeon
open Timer
open Levenshtein
open Magic_numbers
open Render_stack
open Monsters
open Item

(* We have tested most of functions that do not require features, such
   as graphics, audio, and state. Dungeon, Timer, Levenshtein, the
   contstants portion of Magic_numbers, Render_stack, Monsters, and Item
   were tested. Of the functions that were tested in the modules listed
   above, all were simple functions like steppiing timer, Levenshtein
   edit distance, and magic constants produced by Magic_number, unlike
   functions in State, which require complex state manipulations. The
   other modules such as ._menu modules, Texturemap, Spiral, Walker,
   Audio, Render, NPC, and State were tested by play testing, as most of
   them can not be unit tested due to their nature; for example
   teturemap must be check with our eyes, since otherwise we wouldn't
   know if it works properly, and audio must be checked with out ears,
   because otherwise we wouldn't know if it works properly. The test
   suite has tested the correctness of the simple functions assuming
   preconditions are met, which takes care of testing anything that
   doesn't involve complex state manipulation, loading data, or anything
   graphics/audio related. However, the complex modules like State,
   Game, and Dungeon do rely on these simple functions, so testing them
   will reach towards correctness of the whole system. We only black-box
   tested; we tested based on function specification alone without
   looking at implementation. *)

let int_tuple_printer (x, y) =
  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let make_dungeon_tuple_test
    (name : string)
    (dungeon : Dungeon.t)
    (f : Dungeon.t -> int * int)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (f dungeon) ~printer:int_tuple_printer

let make_dungeon_int_test
    (name : string)
    (dungeon : Dungeon.t)
    (f : Dungeon.t -> int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (f dungeon) ~printer:string_of_int

let make_tile_material_test name dungeon location expected_output =
  name >:: fun _ ->
  let cells = get_cells dungeon in
  assert_equal expected_output
    (Hashtbl.find cells location |> get_tile |> tile_material)
    ~printer:(fun a -> a)

let make_dungeon_bool_test
    (name : string)
    (dungeon : Dungeon.t)
    (location : int * int)
    (f : Dungeon.t -> int * int -> bool)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (f dungeon location)
    ~printer:string_of_bool

let make_levenshtein_test
    (name : string)
    (s1 : string)
    (s2 : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Levenshtein.dist s1 s2)
    ~printer:string_of_int

let make_player_location_test
    (name : string)
    (c : State.current)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.player_loc c)
    ~printer:int_tuple_printer

let make_in_fight_test
    (name : string)
    (c : State.current)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.in_fight c)
    ~printer:string_of_bool

let make_typing_test name c ch expected_output =
  name >:: fun _ ->
  assert_equal expected_output (State.fighting_case c ch)

let make_timer_test name step id expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( if step then Timer.step_timer ();
      Timer.current_time id )

let make_animation_test name step id expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( if step then Spriteanimation.step_animation ();
      Spriteanimation.get_sprite id )

let make_render_stack_test name push scene pop expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( if push then Render_stack.stack_push scene
      else if pop then Render_stack.stack_pop ();
      Render_stack.stack_peek () )

let make_magic_number_constant_test name constant expected_output =
  name >:: fun _ -> assert_equal constant expected_output

let make_monster_int_test name monster func expected_output =
  name >:: fun _ ->
  assert_equal expected_output (func monster) ~printer:string_of_int

let make_monster_str_test name monster func expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (List.mem (func monster) monster.attack_strings)

let make_item_test name item func expected_output =
  name >:: fun _ -> assert_equal expected_output (func item)

let dungeon_20x50 =
  Dungeon.instantiate_dungeon ~seed:123 1 20 50 (1, 1) 5 0 0

let dungeon_2x5 =
  Dungeon.instantiate_dungeon ~seed:241 1 2 5 (1, 1) 5 5 10

(* let current_sample_game = State.init_state "sample_game.json" *)

let dungeon_tests =
  [
    make_dungeon_tuple_test "20x50 dungeon dimension 20 by 50"
      dungeon_20x50 get_dimensions (20, 50);
    make_dungeon_tuple_test "2x5 dungeon dimension is 2 by 5"
      dungeon_2x5 get_dimensions (2, 5);
    make_dungeon_tuple_test "start location is (1,1)" dungeon_2x5
      get_start (1, 1);
    make_dungeon_tuple_test "start location is still (1,1)"
      dungeon_20x50 get_start (1, 1);
    make_dungeon_int_test "2x5 dungeon bound is 5" dungeon_2x5 get_bound
      5;
    make_dungeon_int_test "2x5 dungeon id is 1" dungeon_2x5 get_id 1;
    make_dungeon_int_test "2x5 dungeon previous is 10" dungeon_2x5
      get_prev 10;
    make_dungeon_int_test "2x5 dungeon next is 5" dungeon_2x5 get_next 5;
    make_dungeon_bool_test "(0,0) is always a wall" dungeon_2x5 (0, 0)
      is_wall true;
    make_dungeon_int_test "20x50 dungeon seed is 123" dungeon_20x50
      get_time 123;
    make_dungeon_int_test "2x5 dungeon seed is 241" dungeon_2x5 get_time
      241;
  ]

let levenshtein_tests =
  [
    make_levenshtein_test "occurence" "occurrence" "occura nce" 2;
    make_levenshtein_test "abra" "abracaadabra" "candelabra" 7;
    make_levenshtein_test "cats" "cats" "cots" 1;
    make_levenshtein_test "empty" "" "many characters" 15;
    make_levenshtein_test "two empty strings" "" "" 0;
  ]

let timer_tests =
  [
    make_timer_test "After initialization timer1 should be 0" false
      "timer1" 0;
    make_timer_test "After initialization timer2 should be 0" false
      "timer2" 0;
    make_timer_test "After initialization timer3 should be 0" false
      "timer3" 0;
    make_timer_test "After stteping once timer1 should be 1" true
      "timer1" 1;
    make_timer_test "After stepping once timer2 should be 2" false
      "timer2" 2;
    make_timer_test "After stepping once timer3 should be max_int" false
      "timer3" 0;
  ]

let sprite_tests =
  [
    make_animation_test "After initialization anim1 should be sprite1"
      false "anim1" "sprite1";
    make_animation_test "After initialization anim2 should be sprite1"
      false "anim2" "sprite1";
    make_animation_test "After stepping once anim1 should be sprite2"
      true "anim1" "sprite2";
    make_animation_test "After stepping once anim2 should be sprite2"
      false "anim2" "sprite2";
    make_animation_test "After stepping twice anim1 should be sprite1"
      true "anim1" "sprite1";
    make_animation_test "After stepping twice anim2 should be sprite3"
      false "anim2" "sprite3";
    make_animation_test "After stepping thrice anim1 should be sprite2"
      true "anim1" "sprite2";
    make_animation_test "After stepping thrice anim2 should be sprite1"
      false "anim2" "sprite1";
  ]

let render_stack_tests =
  [
    make_render_stack_test
      "the top after initialization should be StartRender" false
      GameoverRender false Render_stack.StartRender;
    make_render_stack_test
      "the top after one pop should be DungeonRender" false
      GameoverRender true Render_stack.DungeonRender;
    make_render_stack_test
      "the top after one push of GameoverRender should be \
       GameoverRender"
      true GameoverRender true Render_stack.GameoverRender;
  ]

let magic_number_tests =
  [
    make_magic_number_constant_test "w should be 500" Magic_numbers.w
      500;
    make_magic_number_constant_test "h should be 500" Magic_numbers.h
      500;
    make_magic_number_constant_test "x_length should be 500"
      Magic_numbers.x_length 11;
    make_magic_number_constant_test "y_length should be 500"
      Magic_numbers.y_length 11;
    make_magic_number_constant_test "width should be 500. /. 11."
      Magic_numbers.width (500. /. 11.);
    make_magic_number_constant_test "height should be 500. /. 11."
      Magic_numbers.width (500. /. 11.);
  ]

let monster =
  {
    name = "fuck";
    sprite = "fuck.png";
    hitpoints = 10;
    encounter_chance = 10;
    attack_strings = [ "fuck"; "you" ];
    max_hp = 10;
  }

let monster_tests =
  [
    make_monster_int_test "monster max hp is 10" monster
      Monsters.get_monster_max_HP 10;
    make_monster_int_test "monster hp at start is 10" monster
      Monsters.get_monster_max_HP 10;
    make_monster_int_test "monster max hp is 10"
      (Monsters.change_monster_hp monster (-1))
      Monsters.get_monster_max_HP 9;
    make_monster_str_test
      "the output of get_monster_string is in monster strings" monster
      Monsters.get_monster_string true;
  ]

let item_stats =
  { sprite = "fuck.png"; name = "fucker"; depth = 1; modifier = 1 }

let weapon = Weapon item_stats

let armor = Armor item_stats

let nothing = NoItem

let item_tests =
  [
    make_item_test "type of weapon is weapon" weapon Item.get_item_type
      "weapon";
    make_item_test "type of armor is armor" armor Item.get_item_type
      "armor";
    make_item_test "type of nothing is noitem" nothing
      Item.get_item_type "none";
    make_item_test "sprite of weapon is fucking.png" weapon
      Item.get_item_sprite "fuck.png";
    make_item_test "name of weapon is fucker" weapon Item.get_item_name
      "fucker";
    make_item_test "depth of weapon is 1" weapon Item.get_item_depth 1;
    make_item_test "modifier of weapon is 1" weapon
      Item.get_item_modifier 1;
  ]

let suite =
  "test suite for CamelQuest"
  >::: List.flatten
         [
           levenshtein_tests;
           timer_tests;
           sprite_tests;
           render_stack_tests;
           magic_number_tests;
           dungeon_tests;
           monster_tests;
           item_tests;
         ]

let _ =
  Timer.init_timers
    [ ("timer1", 1); ("timer2", 2); ("timer3", max_int); ("big", 1) ];
  Spriteanimation.init_animations
    [
      ("anim1", [ "sprite1"; "sprite2" ]);
      ("anim2", [ "sprite1"; "sprite2"; "sprite3" ]);
    ];
  run_test_tt_main suite
