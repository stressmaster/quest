open OUnit2
open Dungeon

let make_dungeon__dimension_test
    (name : string)
    (dungeon : Dungeon.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_dimensions dungeon)

let dungeon_20x50 = Dungeon.instantiate_dungeon 20 50

let dungeon_tests =
  [
    make_dungeon__dimension_test "20x50 dungeon dimension" dungeon_20x50
      (20, 50);
  ]

let state_tests = []

let suite =
  "test suite for CamelQuest"
  >::: List.flatten [ dungeon_tests; state_tests ]

let _ = run_test_tt_main suite
