open OUnit2
open Poker.Cards
open Poker.Actions

let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main tests
