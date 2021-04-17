open OUnit2

let make_dungeon__dimension_test
    (name : string)
    (dungeon : Dungeon.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Dungeon.get_dimensions dungeon)

let make_levenshtein_dist_test
    (name : string)
    (str1 : string)
    (str2 : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Levenshtein.dist str1 str2)

let dungeon_20x50 =
  Dungeon.instantiate_dungeon 0 20 50 (1, 1) (18, 48) 10 [] 0 3

let dungeon_tests =
  [
    make_dungeon__dimension_test "20x50 dungeon dimension" dungeon_20x50
      (20, 50);
  ]

let levenshtein_tests =
  [ make_levenshtein_dist_test "two cases" "occurrence" "occurance" 2 ]

let suite =
  "test suite for CamelQuest"
  >::: List.flatten [ dungeon_tests; levenshtein_tests ]

let _ = run_test_tt_main suite
