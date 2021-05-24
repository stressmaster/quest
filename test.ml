open OUnit2
open Dungeon
open Timer
open Levenshtein
open State
open Magic_numbers
open Render_stack

let int_tuple_printer (x, y) =
  "(" ^ string_of_int x ^ "," ^ string_of_int x ^ ")"

let make_dungeon__tuple_test
    (name : string)
    (dungeon : Dungeon.t)
    (f : Dungeon.t -> int * int)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (f dungeon) ~printer:int_tuple_printer

let make_dungeon__int_test
    (name : string)
    (dungeon : Dungeon.t)
    (f : Dungeon.t -> int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (f dungeon) ~printer:string_of_int

(* let make_monster_hp_test (name : string) (mon : Monster.t)
   (expected_output : int) : test = name >:: fun _ -> assert_equal
   expected_output (get_monster_HP mon) ~printer:string_of_int *)

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

let make_monster_int_test name func monster expected_output =
  name >:: fun _ -> assert_equal expected_output (func monster)

let make_monster_str_test name func monster expected_output =
  name >:: fun _ -> assert_equal expected_output (func monster)

let make_render_stack_test name top expected_output =
  name >:: fun _ -> assert_equal expected_output top

(* let dungeon_20x50 = Dungeon.instantiate_dungeon 1 50 30 (1, 1) 5 [] 0
   0 *)

(*let dungeon_2x2 = Dungeon.instantiate_dungeon 1 2 2 (1, 1) 5 [] 5 10*)

(* let monster_20_hp = Dungeon.instantiate_monster "dog" "dog.pn" 20 59
   [ "a" ] *)

(* let current_sample_game = State.init_state "sample_game.json" *)

(* let dungeon_tests = [ make_dungeon__tuple_test "20x50 dungeon
   dimension" dungeon_20x50 get_dimensions (50, 30);
   make_dungeon__tuple_test "2x5 dungeon dimension" dungeon_2x2
   get_dimensions (2, 2); make_dungeon__int_test "2x5 dungeon bound"
   dungeon_2x2 get_bound 5; make_dungeon__int_test "2x5 dungeon id"
   dungeon_2x2 get_id 1; make_dungeon__int_test "2x5 dungeon previous"
   dungeon_2x2 get_prev 10; make_dungeon__int_test "2x5 dungeon next"
   dungeon_2x2 get_next 5; make_monster_hp_test "20 hp monster "
   monster_20_hp 20; ] *)

let levenshtein_tests =
  [
    make_levenshtein_test "occurence" "occurrence" "occura nce" 2;
    make_levenshtein_test "abra" "abracaadabra" "candelabra" 7;
    make_levenshtein_test "cats" "cats" "cots" 1;
    make_levenshtein_test "empty" "" "many characters" 15;
    make_levenshtein_test "two empty strings" "" "" 0;
  ]

(* let state_tests = [ make_player_location_test "beginning"
   current_sample_game (1, 1); make_in_fight_test "sample game not in
   fight" current_sample_game false; make_typing_test "correct string
   output after enter" current_sample_game 13 ""; make_typing_test
   "ensure correct string output with lowercase letter"
   current_sample_game 99 "c"; make_typing_test "ensure correct string
   output with backspace" current_sample_game 127 ""; make_typing_test
   "ensure correct string output with uppercase letter"
   current_sample_game 65 "A"; make_typing_test "ensure correct string
   output with space" current_sample_game 32 " "; ] *)

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

let monster_tests = []

let render_stack_tests =
  [
    make_render_stack_test
      "the top after initialization should be StartRender"
      (Render_stack.stack_peek ())
      StartRender;
    make_render_stack_test
      "the top after one pop should be DungeonRender"
      ( Render_stack.stack_pop ();
        Render_stack.stack_peek () )
      DungeonRender;
  ]

let suite =
  "test suite for CamelQuest"
  >::: List.flatten
         [
           levenshtein_tests;
           timer_tests;
           sprite_tests;
           render_stack_tests;
         ]

let _ =
  Timer.init_timers
    [ ("timer1", 1); ("timer2", 2); ("timer3", max_int) ];
  Spriteanimation.init_animations
    [
      ("anim1", [ "sprite1"; "sprite2" ]);
      ("anim2", [ "sprite1"; "sprite2"; "sprite3" ]);
    ];
  run_test_tt_main suite
