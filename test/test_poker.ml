open OUnit2
open Poker.Players
open Poker.Cards
open Poker.Actions
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

let test_deck = create_deck ()

let make_deck_length_test _ =
  assert_equal 52 (List.length test_deck) ~msg:"Deck should contain 52 "

let test_draw_empty_deck _ =
  assert_raises (Failure "Deck is empty") (fun () -> draw [])

let test_string_of_card _ =
  let card = find_card Heart Ace (create_deck ()) in
  assert_equal ~msg:"String of Ace of Hearts should be 'A♥'" "A♥"
    (string_of_card card)

let draw_two_cards_test _ =
  let draw1 = draw test_deck in
  let draw2 = draw (snd draw1) in
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

let test_all_suits_2 _ =
  let suits = all_suits () in
  assert_equal ~msg:"Expected 4 suits in all_suits" 4 (List.length suits);
  assert_bool "Spade should be in all suits" (List.mem Spade suits);
  assert_bool "Heart should be in all suits" (List.mem Heart suits);
  assert_bool "Clover should be in all suits" (List.mem Clover suits);
  assert_bool "Club should be in all suits" (List.mem Club suits)

let test_all_ranks_2 _ =
  let ranks = all_ranks () in
  assert_equal ~msg:"Expected 13 ranks in all_ranks" 13 (List.length ranks);
  assert_bool "Two should be in all ranks" (List.mem Two ranks);
  assert_bool "Ace should be in all ranks" (List.mem Ace ranks)

let test_create_deck_2 _ =
  let deck = create_deck () in
  assert_equal ~msg:"Expected deck to have 52 cards" 52 (List.length deck);
  let unique_cards = List.sort_uniq compare deck in
  assert_equal ~msg:"Expected all 52 cards in the deck to be unique" 52
    (List.length unique_cards)

let test_random_two_cards_2 _ =
  let card1, card2 = random_two_cards () in
  assert_bool "First random card should be valid"
    (List.mem card1 (create_deck ()));
  assert_bool "Second random card should be valid"
    (List.mem card2 (create_deck ()))

let test_draw_2 _ =
  let deck = create_deck () in
  let card, new_deck = draw deck in
  assert_bool "Drawn card should be from the deck" (List.mem card deck);
  assert_equal ~msg:"Expected new deck to have 51 cards after a draw" 51
    (List.length new_deck);
  assert_bool "Drawn card should not be in the new deck"
    (not (List.mem card new_deck))

let test_string_of_card_2 _ =
  let (card : Poker.Cards.t) = Poker.Cards.create_card "spade" "ace" in
  let card_str = string_of_card card in
  assert_equal ~msg:"Expected string representation of Ace of Spades to be A♠"
    "A♠" card_str

let test_match_rank _ =
  assert_equal ~msg:"Expected rank to be Two" Two (match_rank "two");
  assert_equal ~msg:"Expected rank to be Three" Three (match_rank "three");
  assert_equal ~msg:"Expected rank to be Four" Four (match_rank "four");
  assert_equal ~msg:"Expected rank to be Five" Five (match_rank "five");
  assert_equal ~msg:"Expected rank to be Six" Six (match_rank "six");
  assert_equal ~msg:"Expected rank to be Seven" Seven (match_rank "seven");
  assert_equal ~msg:"Expected rank to be Eight" Eight (match_rank "eight");
  assert_equal ~msg:"Expected rank to be Nine" Nine (match_rank "nine");
  assert_equal ~msg:"Expected rank to be Ten" Ten (match_rank "ten");
  assert_equal ~msg:"Expected rank to be Jack" Jack (match_rank "jack");
  assert_equal ~msg:"Expected rank to be Queen" Queen (match_rank "queen");
  assert_equal ~msg:"Expected rank to be King" King (match_rank "king");
  assert_equal ~msg:"Expected rank to be Ace" Ace (match_rank "ace");
  assert_raises (Invalid_argument "Incorrect Card Rank") (fun () ->
      match_rank "invalid_rank")

let test_match_suit _ =
  assert_equal ~msg:"Expected suit to be Spade" Spade (match_suit "spade");
  assert_equal ~msg:"Expected suit to be Heart" Heart (match_suit "heart");
  assert_equal ~msg:"Expected suit to be Clover" Clover (match_suit "clover");
  assert_equal ~msg:"Expected suit to be Club" Club (match_suit "club");
  assert_raises (Invalid_argument "Incorrect Card Suit") (fun () ->
      match_suit "invalid_suit")

let test_create_card_and_match _ =
  let deck = create_deck () in
  assert_bool "Deck should contain (Spade, Ace)"
    (List.mem (Poker.Cards.create_card "spade" "ace") deck);
  assert_bool "Deck should contain (Heart, Two)"
    (List.mem (Poker.Cards.create_card "heart" "two") deck);
  assert_bool "Deck should contain (Club, Ten)"
    (List.mem (Poker.Cards.create_card "club" "ten") deck);
  assert_bool "Deck should contain (Clover, Queen)"
    (List.mem (Poker.Cards.create_card "clover" "queen") deck);
  assert_bool "Deck should contain (Spade, King)"
    (List.mem (Poker.Cards.create_card "spade" "king") deck);
  assert_bool "Deck should contain (Heart, Jack)"
    (List.mem (Poker.Cards.create_card "heart" "jack") deck);
  assert_bool "Deck should contain (Club, Four)"
    (List.mem (Poker.Cards.create_card "club" "four") deck);
  assert_bool "Deck should contain (Clover, Nine)"
    (List.mem (Poker.Cards.create_card "clover" "nine") deck);
  assert_equal ~msg:"Deck should contain 52 unique cards" 52
    (List.length (List.sort_uniq compare deck))

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
  let deck = create_deck () in
  let rec draw_all_cards deck acc =
    if size deck = 0 then List.rev acc
    else
      let card, new_deck = draw deck in
      draw_all_cards new_deck (card :: acc)
  in
  let all_cards = draw_all_cards deck [] in
  assert_equal 52 (List.length all_cards) ~msg:"The deck should contain 52 ";
  List.iter
    (fun card ->
      let card_str = string_of_card card in
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

(* actions.ml *)
let simulate_input inputs f =
  let original_stdin = Unix.dup Unix.stdin in
  let temp_read, temp_write = Unix.pipe () in
  Unix.dup2 temp_read Unix.stdin;
  let write_inputs () =
    List.iter
      (fun input ->
        let input_bytes = Bytes.of_string (input ^ "\n") in
        let _ =
          Unix.write temp_write input_bytes 0 (Bytes.length input_bytes)
        in
        ())
      inputs
  in
  write_inputs ();
  let result = f () in
  Unix.close temp_read;
  Unix.close temp_write;
  Unix.dup2 original_stdin Unix.stdin;
  Unix.close original_stdin;
  result

let setup_game_state () =
  let players, deck = create_players_with_names [ "Alice"; "Bob" ] in
  let state = create_state players deck in
  (state, List.hd players)

let test_valid_call _ =
  simulate_input [ "call" ] (fun () ->
      let state, player = setup_game_state () in
      assert_equal Call
        (action player state [ "call" ])
        ~msg:"Should accept valid call action")

let test_valid_check _ =
  simulate_input [ "check" ] (fun () ->
      let state, player = setup_game_state () in
      set_contributions player (get_current_bet state);
      assert_equal Check
        (action player state [ "check" ])
        ~msg:"Should accept valid check action")

let test_valid_fold _ =
  simulate_input [ "fold" ] (fun () ->
      let state, player = setup_game_state () in
      assert_equal Fold
        (action player state [ "fold" ])
        ~msg:"Should accept valid fold action")

let test_valid_raise _ =
  simulate_input [ "raise"; "10" ] (fun () ->
      let state, player = setup_game_state () in
      assert_equal (Raise 10)
        (action player state [ "raise" ])
        ~msg:"Should accept valid raise action with amount")

let test_invalid_check_with_unmatched_contribution _ =
  simulate_input [ "check"; "fold" ] (fun () ->
      let state, player = setup_game_state () in
      raise_bet state player 10;
      set_contributions player 0;
      assert_equal Fold
        (action player state [ "check"; "fold" ])
        ~msg:"Should reject check when contribution doesn't match current bet")

let test_invalid_action_then_valid _ =
  simulate_input [ "invalid_action"; "fold" ] (fun () ->
      let state, player = setup_game_state () in
      assert_equal Fold
        (action player state [ "fold" ])
        ~msg:"Should reject invalid action and accept subsequent valid action")

let tests =
  "Poker Test Suite"
  >::: [
         "test_all_suits" >:: test_all_suits;
         "test_all_ranks" >:: test_all_ranks;
         "make_deck_length" >:: make_deck_length_test;
         "draw_two_cards_test" >:: draw_two_cards_test;
         "test_create_deck" >:: test_create_deck;
         "test_random_two_cards" >:: test_random_two_cards;
         "test_all_suits_2" >:: test_all_suits_2;
         "test_all_ranks_2" >:: test_all_ranks_2;
         "test_create_deck_2" >:: test_create_deck_2;
         "test_random_two_cards_2" >:: test_random_two_cards_2;
         "test_string_of_card_2" >:: test_string_of_card_2;
         "test_draw" >:: test_draw;
         "test_draw_2" >:: test_draw_2;
         "test_match_rank" >:: test_match_rank;
         "test_match_suit" >:: test_match_suit;
         "test_create_card_and_match" >:: test_create_card_and_match;
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
         "test_valid_call" >:: test_valid_call;
         "test_valid_check" >:: test_valid_check;
         "test_valid_fold" >:: test_valid_fold;
         "test_valid_raise" >:: test_valid_raise;
         "test_invalid_check_with_unmatched_contribution"
         >:: test_invalid_check_with_unmatched_contribution;
         "test_invalid_action_then_valid" >:: test_invalid_action_then_valid;
       ]

let () = run_test_tt_main tests
