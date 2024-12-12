open Poker.Players
open Poker.Cards
open Poker.Actions
open Poker.State

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
    in a round. The state is updated after every turn. *)
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
  print_three_card flop;
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
  print_four_card (get_community_cards state);
  print_endline "=====================================================";

  (* Another betting phase after the turn *)
  bet_round state;

  (* Reveal the river (5th community card) *)
  let deck = get_deck state in
  let river, _ = draw deck in
  set_community_cards state [ river ];
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "========================================================";
  print_five_card (get_community_cards state);
  print_endline "========================================================";

  (* Final betting phase after the river *)
  bet_round state;

  print_endline "Enter who the winner is: ";

  let winner = read_line () in
  Printf.printf "\nCongrats to %s!\n" winner;
  Printf.printf "You have won %d!\n\n" (get_pot state)

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
        Poker.Cards.print_two_card card1 card2;
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
