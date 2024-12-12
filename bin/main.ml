open Poker.Players
open Poker.Cards
open Poker.Actions
open Poker.State

type hand_rank =
  | RoyalFlush
  | StraightFlush of rank
  | FourOfAKind of rank * rank
  | FullHouse of rank * rank
  | Flush of rank list
  | Straight of rank
  | ThreeOfAKind of rank * rank list
  | TwoPair of rank * rank * rank
  | OnePair of rank * rank list
  | HighCard of rank list

let rank_value = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let compare_ranks r1 r2 = compare (rank_value r1) (rank_value r2)

(** [sort_by_rank cards] sorts [cards] by ascending rank. *)
let sort_by_rank cards =
  List.sort (fun c1 c2 -> compare_ranks (get_rank c1) (get_rank c2)) cards

(** [is_flush cards] checks if [cards] all share the same suit (flush). *)
let is_flush cards =
  match cards with
  | [] -> false
  | c :: cs ->
      let s = get_suit c in
      List.for_all (fun x -> get_suit x = s) cs

(** [is_straight ranks] checks if the [ranks] forms a regular straight *)
let is_straight ranks =
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      let v1, v2, v3, v4, v5 =
        ( rank_value r1,
          rank_value r2,
          rank_value r3,
          rank_value r4,
          rank_value r5 )
      in
      v2 = v1 + 1 && v3 = v2 + 1 && v4 = v3 + 1 && v5 = v4 + 1
  | _ -> false

(** [is_wheel_straight ranks] checks if [ranks] is a specific corner case *)
let is_wheel_straight ranks =
  match ranks with
  | [ Ace; Five; Four; Three; Two ] -> true
  | _ -> false

(** [rank_frequencies cards] counts the frequency of each in the [cards]. *)
let rank_frequencies cards =
  let tbl = Hashtbl.create 7 in
  List.iter
    (fun c ->
      let r = get_rank c in
      Hashtbl.replace tbl r
        (1 + (Hashtbl.find_opt tbl r |> Option.value ~default:0)))
    cards;
  let freq_list = Hashtbl.fold (fun r count acc -> (r, count) :: acc) tbl [] in
  List.sort
    (fun (r1, c1) (r2, c2) ->
      let c = compare c2 c1 in
      if c <> 0 then c else compare_ranks r2 r1)
    freq_list

(* extract ranks in descending order *)
let extract_ranks_desc cards =
  let ranks = List.map get_rank cards in
  List.sort (fun a b -> compare_ranks b a) ranks

(* main hand evaluation function *)
let evaluate_hand cards =
  if List.length cards <> 5 then
    invalid_arg "evaluate_hand: must provide exactly 5 cards";
  let sorted_cards = sort_by_rank cards in
  let ranks_asc = List.map get_rank sorted_cards in
  let flush_flag = is_flush sorted_cards in
  let straight_flag = is_straight ranks_asc || is_wheel_straight ranks_asc in
  let freq = rank_frequencies sorted_cards in

  match freq with
  | [ (r1, 4); (r2, 1) ] -> FourOfAKind (r1, r2)
  | [ (r1, 3); (r2, 2) ] -> FullHouse (r1, r2)
  | _ when flush_flag && straight_flag ->
      let ranks_desc = List.rev ranks_asc in
      let rv = List.map rank_value ranks_desc in
      if rv = [ 10; 11; 12; 13; 14 ] then RoyalFlush
      else
        let high =
          if is_wheel_straight ranks_asc then Five else List.nth ranks_asc 4
        in
        StraightFlush high
  | _ when flush_flag -> Flush (extract_ranks_desc sorted_cards)
  | _ when straight_flag ->
      let high =
        if is_wheel_straight ranks_asc then Five else List.nth ranks_asc 4
      in
      Straight high
  | [ (r1, 3); (r2, 1); (r3, 1) ] ->
      let kickers = List.sort (fun a b -> compare_ranks b a) [ r2; r3 ] in
      ThreeOfAKind (r1, kickers)
  | [ (r1, 2); (r2, 2); (r3, 1) ] ->
      let pair_ranks = [ r1; r2 ] |> List.sort (fun a b -> compare_ranks b a) in
      TwoPair (List.nth pair_ranks 0, List.nth pair_ranks 1, r3)
  | [ (r1, 2); (r2, 1); (r3, 1); (r4, 1) ] ->
      let kickers =
        [ r2; r3; r4 ] |> List.sort (fun a b -> compare_ranks b a)
      in
      OnePair (r1, kickers)
  | _ -> HighCard (extract_ranks_desc sorted_cards)

(** [compare_hand_rank h1 h2] compares the hand rank values of [h1] and [h2] and
    determines which is better. Returns > 0 if h1 is better than h2, 0 if equal,
    < 0 if h1 is worse than h2 *)
let compare_hand_rank h1 h2 =
  let rank_category = function
    | RoyalFlush -> 9
    | StraightFlush _ -> 8
    | FourOfAKind _ -> 7
    | FullHouse _ -> 6
    | Flush _ -> 5
    | Straight _ -> 4
    | ThreeOfAKind _ -> 3
    | TwoPair _ -> 2
    | OnePair _ -> 1
    | HighCard _ -> 0
  in

  let c1 = rank_category h1 in
  let c2 = rank_category h2 in
  if c1 <> c2 then compare c1 c2
  else
    let compare_descending_ranks l1 l2 =
      let rec aux = function
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys ->
            let cmp = compare_ranks x y in
            if cmp <> 0 then cmp else aux (xs, ys)
      in
      aux (l1, l2)
    in
    match (h1, h2) with
    | RoyalFlush, RoyalFlush -> 0
    | StraightFlush r1, StraightFlush r2 -> compare_ranks r1 r2
    | FourOfAKind (r1, k1), FourOfAKind (r2, k2) ->
        let cmp = compare_ranks r1 r2 in
        if cmp = 0 then compare_ranks k1 k2 else cmp
    | FullHouse (t1, p1), FullHouse (t2, p2) ->
        let cmp = compare_ranks t1 t2 in
        if cmp = 0 then compare_ranks p1 p2 else cmp
    | Flush l1, Flush l2 -> compare_descending_ranks l1 l2
    | Straight r1, Straight r2 -> compare_ranks r1 r2
    | ThreeOfAKind (t1, ks1), ThreeOfAKind (t2, ks2) ->
        let cmp = compare_ranks t1 t2 in
        if cmp = 0 then compare_descending_ranks ks1 ks2 else cmp
    | TwoPair (h1a, h1b, k1), TwoPair (h2a, h2b, k2) ->
        let cmp = compare_ranks h1a h2a in
        if cmp <> 0 then cmp
        else
          let cmp2 = compare_ranks h1b h2b in
          if cmp2 <> 0 then cmp2 else compare_ranks k1 k2
    | OnePair (p1, ks1), OnePair (p2, ks2) ->
        let cmp = compare_ranks p1 p2 in
        if cmp = 0 then compare_descending_ranks ks1 ks2 else cmp
    | HighCard ks1, HighCard ks2 -> compare_descending_ranks ks1 ks2
    | _ -> 0

(** [combinations_7_choose_5] generates every possible combination of 5 cards in
    the 7 total [cards]. *)
let combinations_7_choose_5 cards =
  let rec choose k lst =
    if k = 0 then [ [] ]
    else
      match lst with
      | [] -> []
      | x :: xs ->
          let with_x = List.map (fun c -> x :: c) (choose (k - 1) xs) in
          let without_x = choose k xs in
          with_x @ without_x
  in
  choose 5 cards

(** [best_hand_of_seven cards7] takes the best 5-card hand of [cards7]*)
let best_hand_of_seven cards7 =
  let all_5_combos = combinations_7_choose_5 cards7 in
  let best =
    List.fold_left
      (fun best_opt combo ->
        let hand = evaluate_hand combo in
        match best_opt with
        | None -> Some hand
        | Some current_best ->
            if compare_hand_rank hand current_best > 0 then Some hand
            else best_opt)
      None all_5_combos
  in
  match best with
  | Some h -> h
  | None -> failwith "No best hand found."

(** [split_at n] is a helper function that splits a [lst] at index [n] *)
let split_at n lst =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | x :: xs when i < n -> aux (i + 1) (x :: acc) xs
    | xs -> (List.rev acc, xs)
  in
  aux 0 [] lst

let bet_round state =
  reset_current_bet state;
  List.iter (fun player -> set_contributions player 0) (get_players state);
  List.iter
    (fun player ->
      print_endline ("It's " ^ get_name player ^ "'s turn:");
      let options = [ "call"; "check"; "fold"; "raise" ] in
      let action = action player state options in
      match action with
      | Call -> call state player (get_current_bet state)
      | Check -> check state player
      | Fold -> fold state player
      | Raise amount -> raise_bet state player amount)
    (get_players state);

  List.iter
    (fun player ->
      if get_current_bet state <> get_contributions player then begin
        print_endline ("It's " ^ get_name player ^ "'s turn to match:");
        let options = [ "call"; "check"; "fold" ] in
        let action = action player state options in
        match action with
        | Call -> call state player (get_current_bet state)
        | Check -> check state player
        | Fold -> fold state player
        | Raise amount -> call state player (get_current_bet state)
      end)
    (get_players state);

  print_endline "\nPress Enter to continue.";
  ignore (read_line ())

(** [start_game player_names] initializes the game with the given list of player
    names and sets up the game state. *)
let start_game player_names =
  let players, deck = create_players_with_names (List.rev player_names) in
  create_state players deck

(** [game_loop state] handles the main gameplay loop, where players take turns
    in a round. The [state] is updated after every turn. *)
let game_loop state =
  let active_players = get_players state in
  if List.length active_players <= 1 then
    match active_players with
    | [ winner ] ->
        Printf.printf "%s is the winner!\n" (get_name winner);
        print_endline "Game over!"
    | _ -> print_endline "Game over!"
  else (*First betting round before community cards *)
    bet_round state;

  (* Reveal the flop (first 3 community cards) *)
  let deck = get_deck state in
  let card1, deck1 = draw deck in
  let card2, deck2 = draw deck1 in
  let card3, deck3 = draw deck2 in
  let flop = [ card1; card2; card3 ] in
  set_community_cards state flop;
  set_deck state deck3;
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "===================================================";
  let printing_card = print_three_card flop in
  print_endline printing_card;
  print_endline "===================================================";

  (* Another betting phase after the flop *)
  bet_round state;

  (* Reveal the turn (4th community card) *)
  let deck = get_deck state in
  let turn, deck4 = draw deck in
  set_community_cards state [ turn ];
  set_deck state deck4;
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "=====================================================";
  let printing_card1, printing_card2 =
    print_four_card (get_community_cards state)
  in
  print_endline printing_card1;
  print_endline printing_card2;
  print_endline "=====================================================";

  (* Another betting phase after the turn *)
  bet_round state;

  (* Reveal the river (5th community card) *)
  let deck = get_deck state in
  let river, _ = draw deck in
  set_community_cards state [ river ];
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "========================================================";
  let printing_card1, printing_card2 =
    print_five_card (get_community_cards state)
  in
  print_endline printing_card1;
  print_endline printing_card2;
  print_endline "========================================================";

  (* Final betting phase after the river *)
  bet_round state;

  print_endline "Enter who the winner is: ";

  bet_round state;

  (* Evaluate all players' hands and determine the winner *)
  let community = get_community_cards state in
  let players = get_players state in
  let player_best_hands =
    List.map
      (fun p ->
        let c1, c2 = get_card p in
        let seven_cards = c1 :: c2 :: community in
        let best = best_hand_of_seven seven_cards in
        (p, best))
      players
  in

  (* Determine the winner(s) by comparing best hands *)
  let best_players, best_hand =
    List.fold_left
      (fun (winners, current_best) (p, h) ->
        match winners with
        | [] -> ([ p ], h)
        | _ ->
            let cmp = compare_hand_rank h current_best in
            if cmp > 0 then ([ p ], h)
            else if cmp = 0 then (p :: winners, current_best)
            else (winners, current_best))
      ([], HighCard []) player_best_hands
  in

  match best_players with
  | [] -> print_endline "No winner found. Something went wrong."
  | [ w ] ->
      Printf.printf "\nCongrats to %s!\n" (get_name w);
      Printf.printf "You have won %d!\n\n" (get_pot state)
  | ws ->
      print_endline "\nIt's a tie!";
      print_endline "The following players tied for the best hand:";
      List.iter (fun p -> Printf.printf "- %s\n" (get_name p)) ws;
      Printf.printf "They split the pot of %d!\n\n" (get_pot state)

(** [clear_screen] adds a large number of blank lines to hide the previous *)
let clear_screen () = print_endline ("\n" ^ String.make 50 '\n')

(** [prompt_num_players] prompts the user to input the number of active players *)
let rec prompt_num_players () =
  print_endline "Enter the number of players (2 to 9):";
  try
    let num_players = int_of_string (read_line ()) in
    if num_players >= 2 && num_players <= 9 then num_players
    else (
      print_endline "Please enter a number between 2 and 9.";
      prompt_num_players ())
  with Failure _ ->
    print_endline "Invalid number. Please enter a valid number.";
    prompt_num_players ()

(** [prompt_name num_players] prompts the name for [num_players] playing *)
let rec prompt_name num_players =
  let rec gather_names names remaining_players =
    if remaining_players = 0 then names
    else begin
      print_endline
        ("\nEnter the name of Player "
        ^ string_of_int (num_players - remaining_players + 1)
        ^ ":");
      let name = read_line () in
      gather_names (name :: names) (remaining_players - 1)
    end
  in
  gather_names [] num_players

(** [reveal_player_cards players] reveal the cards for each of the active
    [players] *)
let reveal_player_cards players =
  let rec reveal_cards players =
    match players with
    | [] -> ()
    | player :: rest ->
        clear_screen ();
        Printf.printf "Player: %s\n" (Poker.Players.get_name player);
        let card1, card2 = Poker.Players.get_card player in
        print_endline "Cards: ";
        let printing_card = Poker.Cards.print_two_card card1 card2 in
        print_endline printing_card;
        print_endline "\nPress Enter to continue.";
        ignore (read_line ());
        clear_screen ();
        if rest <> [] then begin
          Printf.printf "Press Enter to see next %s's card..."
            (Poker.Players.get_name (List.hd rest));
          ignore (read_line ());
          reveal_cards rest
        end
        else reveal_cards rest
  in
  reveal_cards players

let rec main () =
  Random.self_init ();
  print_endline "\nWelcome to Poker!";
  print_endline "Do you want to play poker? (y/n)";
  let user_input = read_line () in
  match user_input with
  | "y" | "Y" ->
      print_endline "\nGreat! Let's start the game.";
      let num_players = prompt_num_players () in
      let player_names = prompt_name num_players in
      let state = start_game player_names in
      let active_players = Poker.State.get_players state in
      Printf.printf "\nPress Enter to see %s's Cards..."
        (Poker.Players.get_name (List.hd active_players));
      ignore (read_line ());
      reveal_player_cards active_players;
      game_loop state
  | "n" | "N" ->
      print_endline "Sorry to see you go.";
      ()
  | _ ->
      print_endline "Invalid input. Please restart the game.";
      main ()

let () = main ()
