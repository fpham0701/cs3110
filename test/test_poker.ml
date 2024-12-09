open OUnit2
open Poker.Players
open Poker.Cards
open Poker.Actions
open Poker.Round
open Poker.State

(* Tests for Card Module *)

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

let make_deck_length_test _ =
  assert_equal 52 (List.length test_deck) ~msg:"Deck should contain 52 cards."

let test_draw_empty_deck _ =
  assert_raises (Failure "Deck is empty") (fun () -> draw [])

let test_string_of_card _ =
  let card =
    find_card Poker.Cards.Heart Poker.Cards.Ace (Poker.Cards.create_deck ())
  in
  assert_equal ~msg:"String of Ace of Hearts should be 'A♥'" "A♥"
    (Poker.Cards.string_of_card card)

let draw_two_cards_test _ =
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

(* Tests for State Module *)

let test_update_pot _ =
  let state = create_state [] [] in
  update_pot state 5;
  assert_equal ~printer:string_of_int 5 (get_pot state)

let test_get_current_bet _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in
  assert_equal ~printer:string_of_int 0 (get_current_bet state);
  let alice = List.hd (get_players state) in
  raise_bet state alice 10;
  assert_equal ~printer:string_of_int 10 (get_current_bet state);
  reset_current_bet state;
  assert_equal ~printer:string_of_int 0 (get_current_bet state)

let test_get_community_cards _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in
  let card1, deck1 = draw deck in
  let card2, deck2 = draw deck1 in
  let card3, _ = draw deck2 in
  let flop = [ card1; card2; card3 ] in
  set_community_cards state flop;
  assert_equal
    ~printer:(fun cards -> String.concat ", " (List.map string_of_card cards))
    flop
    (get_community_cards state)

let test_call _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in
  let alice = List.hd (get_players state) in
  call state alice 50;
  assert_equal ~printer:string_of_int 50 (get_contributions alice);
  assert_equal ~printer:string_of_int 50 (get_pot state)

let test_fold _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob"; "Charlie" ] in
  let state = create_state players deck in
  let alice = List.hd (get_players state) in
  fold state alice;
  assert_bool "Alice should be removed from the players"
    (not (List.exists (fun p -> get_name p = "Alice") (get_players state)));
  let bob = List.hd (get_players state) in
  fold state bob;
  let remaining_player = List.hd (get_players state) in
  assert_equal "Charlie"
    (get_name remaining_player)
    ~msg:"Charlie should be the winner"

let test_check _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in
  let alice = List.hd (get_players state) in
  raise_bet state alice 10;
  set_contributions alice 10;
  assert_equal () (check state alice);
  let bob = List.nth (get_players state) 1 in
  assert_raises
    (Invalid_argument "Player's contribution does not match the current bet")
    (fun () -> check state bob)

let test_get_cards _ =
  let players, deck = create_players_with_names [ "Alice" ] in
  let alice = List.hd players in
  let cards = get_card alice in
  let card1, card2 = cards in
  assert_bool "Alice's first card" (List.mem card1 deck);
  assert_bool "Alice's second card" (List.mem card2 deck);
  assert_bool "The two cards should be different" (card1 <> card2)

let print_players_info player_names =
  let players, _ = create_players_with_names player_names in
  List.iter print_player players

let test_get_deck _ =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in

  let initial_deck = get_deck state in
  assert_equal 52 (List.length initial_deck);
  let card1, updated_deck = draw initial_deck in
  set_deck state updated_deck;
  let deck_after_one_draw = get_deck state in
  assert_equal 51 (List.length deck_after_one_draw);
  assert_bool "First drawn card should not be in the deck"
    (not (List.mem card1 deck_after_one_draw));

  let card2, updated_deck = draw deck_after_one_draw in
  set_deck state updated_deck;
  let deck_after_two_draws = get_deck state in
  assert_equal 50 (List.length deck_after_two_draws);
  assert_bool "Second drawn card should not be in the deck"
    (not (List.mem card2 deck_after_two_draws))

let test_draw_all_cards_and_validate _ =
  let deck = Poker.Cards.create_deck () in
  let rec draw_all_cards deck acc =
    if Poker.Cards.size deck = 0 then List.rev acc
    else
      let card, new_deck = Poker.Cards.draw deck in
      draw_all_cards new_deck (card :: acc)
  in
  let all_cards = draw_all_cards deck [] in
  assert_equal 52 (List.length all_cards)
    ~msg:"The deck should contain 52 cards.";
  List.iter
    (fun card ->
      let card_str = Poker.Cards.string_of_card card in
      assert_bool
        ("String representation of card " ^ card_str ^ " is not empty")
        (String.length card_str > 0))
    all_cards

let test_rank_to_string _ =
  assert_equal "2" (rank_to_string Two);
  assert_equal "3" (rank_to_string Three);
  assert_equal "4" (rank_to_string Four);
  assert_equal "5" (rank_to_string Five);
  assert_equal "6" (rank_to_string Six);
  assert_equal "7" (rank_to_string Seven);
  assert_equal "8" (rank_to_string Eight);
  assert_equal "9" (rank_to_string Nine);
  assert_equal "10" (rank_to_string Ten);
  assert_equal "J" (rank_to_string Jack);
  assert_equal "Q" (rank_to_string Queen);
  assert_equal "K" (rank_to_string King);
  assert_equal "A" (rank_to_string Ace)

let test_suit_to_symbol _ =
  assert_equal "♠" (suit_to_symbol Spade);
  assert_equal "♥" (suit_to_symbol Heart);
  assert_equal "♣" (suit_to_symbol Clover);
  assert_equal "♦" (suit_to_symbol Club)

(* round.ml *)

let tests =
  "Poker Test Suite"
  >::: [
         "test_all_suits" >:: test_all_suits;
         "test_all_ranks" >:: test_all_ranks;
         "make_deck_length" >:: make_deck_length_test;
         "draw_two_cards_test" >:: draw_two_cards_test;
         "test_create_deck" >:: test_create_deck;
         "test_random_two_cards" >:: test_random_two_cards;
         "test_draw" >:: test_draw;
         "test_size" >:: test_size;
         "test_update_pot" >:: test_update_pot;
         "test_current_bet" >:: test_get_current_bet;
         "test_get_community_cards" >:: test_get_community_cards;
         "test_call" >:: test_call;
         "test_fold" >:: test_fold;
         "test_check" >:: test_check;
         "test_get_cards" >:: test_get_cards;
         "test_get_deck" >:: test_get_deck;
         "test_draw_empty_deck" >:: test_draw_empty_deck;
         "test_string_of_card" >:: test_string_of_card;
         "test_multiple_draws" >:: test_multiple_draws;
         "test_all_suits_correctness" >:: test_all_suits_correctness;
         "test_all_ranks_correctness" >:: test_all_ranks_correctness;
         "test_draw_all_cards_and_validate" >:: test_draw_all_cards_and_validate;
         "test_rank_to_string" >:: test_rank_to_string;
         "test_suit_to_symbol" >:: test_suit_to_symbol;
       ]

let () = run_test_tt_main tests
