open OUnit2
open Dungeon

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

let make_monster_hp_test
    (name : string)
    (mon : Dungeon.monster)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_monster_HP mon)
    ~printer:string_of_int

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
  assert_equal expected_output (State.typing_case c ch)

let dungeon_20x50 = Dungeon.instantiate_dungeon 1 50 30 (1, 1) 5 [] 0 0

let dungeon_2x2 = Dungeon.instantiate_dungeon 1 2 2 (1, 1) 5 [] 5 10

let monster_20_hp =
  Dungeon.instantiate_monster "dog" "dog.pn" 20 59 [ "a" ]

let current_sample_game = State.init_state "sample_game.json"

let dungeon_tests =
  [
    make_dungeon__tuple_test "20x50 dungeon dimension" dungeon_20x50
      get_dimensions (50, 30);
    make_dungeon__tuple_test "2x5 dungeon dimension" dungeon_2x2
      get_dimensions (2, 2);
    make_dungeon__int_test "2x5 dungeon bound" dungeon_2x2 get_bound 5;
    make_dungeon__int_test "2x5 dungeon id" dungeon_2x2 get_id 1;
    make_dungeon__int_test "2x5 dungeon previous" dungeon_2x2 get_prev
      10;
    make_dungeon__int_test "2x5 dungeon next" dungeon_2x2 get_next 5;
    make_monster_hp_test "20 hp monster " monster_20_hp 20;
  ]

let levenshtein_tests =
  [
    make_levenshtein_test "occurence" "occurrence" "occura nce" 2;
    make_levenshtein_test "abra" "abracaadabra" "candelabra" 7;
    make_levenshtein_test "cats" "cats" "cots" 1;
    make_levenshtein_test "empty" "" "many characters" 15;
    make_levenshtein_test "two empty strings" "" "" 0;
  ]

let state_tests =
  [
    make_player_location_test "beginning" current_sample_game (1, 1);
    make_in_fight_test "sample game not in fight" current_sample_game
      false;
    make_typing_test "correct string output after enter"
      current_sample_game 13 "";
    make_typing_test
      "ensure correct string output with lowercase letter"
      current_sample_game 99 "c";
    make_typing_test "ensure correct string output with backspace"
      current_sample_game 127 "";
    make_typing_test
      "ensure correct string output with uppercase letter"
      current_sample_game 65 "A";
    make_typing_test "ensure correct string output with space"
      current_sample_game 32 " ";
  ]

let suite =
  "test suite for CamelQuest"
  >::: List.flatten [ dungeon_tests; levenshtein_tests; state_tests ]

let _ = run_test_tt_main suite
