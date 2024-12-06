open OUnit2
open Poker.Players
open Poker.Cards

let test_all_suits _ =
  assert_equal ~msg:"All suits should be four" 4 (List.length (all_suits ()))

let rec find_card suit rank deck =
  match deck with
  | [] -> failwith "Card not found"
  | card :: rest ->
      if get_suit card = suit && get_rank card = rank then card
      else find_card suit rank rest

let test_all_ranks _ =
  assert_equal ~msg:"All ranks should be thirteen" 13
    (List.length (all_ranks ()))

let test_deck = Poker.Cards.create_deck ()

(** [make_deck_length_test] checks that the deck has 52 unique cards. *)
let make_deck_length_test =
  "52 unique cards" >:: fun _ ->
  assert_equal 52 (List.length test_deck) ~msg:"Deck should contain 52 cards."

let test_draw_empty_deck _ =
  assert_raises (Failure "Deck is empty") (fun () -> draw [])

let test_string_of_card _ =
  let card =
    find_card Poker.Cards.Heart Poker.Cards.Ace (Poker.Cards.create_deck ())
  in
  assert_equal ~msg:"String of Ace of Hearts should be 'A♥'" "A♥"
    (Poker.Cards.string_of_card card)

(** [draw_two_cards_test] checks that two draws cards are unique and valid. *)
let draw_two_cards_test =
  "Random two cards test" >:: fun _ ->
  let draw1 = Poker.Cards.draw test_deck in
  let draw2 = Poker.Cards.draw (snd draw1) in
  let card1 = fst draw1 in
  let card2 = fst draw2 in
  let new_deck = snd draw2 in
  assert_bool "The two cards should be different" (card1 <> card2);
  assert_bool "First card should no longer be in the deck"
    (not (List.mem card1 new_deck));
  assert_bool "Second card should no longer be in the deck"
    (not (List.mem card2 new_deck));
  assert_bool "The deck should have 50 cards" (List.length new_deck = 50)

let test_create_deck _ =
  let deck = create_deck () in
  assert_equal ~msg:"Deck size should be 52" 52 (size deck);
  assert_bool "Deck should contain unique cards"
    (List.length deck = List.length (List.sort_uniq compare deck))

let test_random_two_cards _ =
  let card1, card2 = random_two_cards () in
  assert_bool "Random cards should be distinct" (card1 <> card2)

let test_draw _ =
  let deck = create_deck () in
  let card, new_deck = draw deck in
  assert_bool "Drawn card should not be in the new deck"
    (not (List.mem card new_deck));
  assert_equal ~msg:"New deck should have one less card"
    (size deck - 1)
    (size new_deck)

let test_multiple_draws _ =
  let deck = create_deck () in
  let _, deck1 = draw deck in
  let _, deck2 = draw deck1 in
  let _, deck3 = draw deck2 in
  assert_equal ~msg:"Deck size after 3 draws should be 49" 49 (size deck3)

let test_size _ =
  let deck = create_deck () in
  assert_equal ~msg:"Initial deck size should be 52" 52 (size deck);
  let _, smaller_deck = draw deck in
  assert_equal ~msg:"Deck size after draw should be 51" 51 (size smaller_deck)

let test_all_suits_correctness _ =
  assert_bool "All suits should contain Spade, Heart, Clover, Club"
    (List.sort compare (all_suits ())
    = List.sort compare [ Spade; Heart; Clover; Club ])

let test_all_ranks_correctness _ =
  assert_bool "All ranks should contain all ranks from Two to Ace"
    (List.sort compare (all_ranks ())
    = List.sort compare
        [
          Two;
          Three;
          Four;
          Five;
          Six;
          Seven;
          Eight;
          Nine;
          Ten;
          Jack;
          Queen;
          King;
          Ace;
        ])

(* Card Test suite *)
let card_tests =
  "Card Test Suite"
  >::: [
         "test_all_suits" >:: test_all_suits;
         "test_all_ranks" >:: test_all_ranks;
         make_deck_length_test;
         draw_two_cards_test;
         "test_create_deck" >:: test_create_deck;
         "test_random_two_cards" >:: test_random_two_cards;
         "test_draw" >:: test_draw;
         "test_size" >:: test_size;
         "test draw empty deck" >:: test_draw_empty_deck;
         "test string of card" >:: test_string_of_card;
         "test multiple draws" >:: test_multiple_draws;
         "test all suits correctness" >:: test_all_suits_correctness;
         "test all ranks correctness" >:: test_all_ranks_correctness;
       ]

let _ = run_test_tt_main card_tests
