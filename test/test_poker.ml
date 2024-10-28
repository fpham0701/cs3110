open OUnit2
open Poker.Players
open Poker.Cards

let test_deck = Poker.Cards.create_deck ()

(** [make_deck_length_test] checks that the deck has 52 unique cards. *)
let make_deck_length_test =
  "52 unique cards" >:: fun _ ->
  assert_equal 52 (List.length test_deck) ~msg:"Deck should contain 52 cards."

(** [make_random_two_cards_test] checks that two random cards are unique and
    valid. *)
let make_random_two_cards_test =
  "Random two cards test" >:: fun _ ->
  let card1, card2 = Poker.Cards.random_two_cards () in
  assert_bool "The two cards should be different" (card1 <> card2);
  let deck = Poker.Cards.create_deck () in
  assert_bool "First card should be in the deck" (List.mem card1 deck);
  assert_bool "Second card should be in the deck" (List.mem card2 deck)

(** [test_create_players] checks that create_players produces 9 players with
    unique names and two distinct cards each. *)
let test_create_players _ =
  let players = Poker.Players.create_players () in
  assert_equal 9 (List.length players) ~msg:"There should be 9 players.";
  let expected_names =
    [
      "Player 1";
      "Player 2";
      "Player 3";
      "Player 4";
      "Player 5";
      "Player 6";
      "Player 7";
      "Player 8";
      "Player 9";
    ]
  in
  let actual_names = List.map Poker.Players.get_name players in
  assert_equal expected_names actual_names
    ~msg:"Player names should match the expected names.";

  List.iter
    (fun player ->
      let card1, card2 = Poker.Players.get_card player in
      assert_bool "Each player should have two distinct cards." (card1 <> card2))
    players

let test_remove_players =
  let players = Poker.Players.create_players () in
  assert_equal 9 (List.length players) ~msg:"There should be 9 players.";

  let removed_one_list =
    Poker.Players.remove_player (List.nth players 0) players
  in
  assert_equal 8
    (List.length removed_one_list)
    ~msg:"There should be 8 players now (removed player 1)";

  let removed_two_list =
    Poker.Players.remove_player (List.nth removed_one_list 4) removed_one_list
  in
  assert_equal 7
    (List.length removed_two_list)
    ~msg:"There should be 7 players now (2 removed)"

(* Test suite *)
let tests =
  "Test suite for Poker module"
  >::: [
         make_deck_length_test;
         make_random_two_cards_test;
         "test_create_players" >:: test_create_players;
       ]

let _ = run_test_tt_main tests
