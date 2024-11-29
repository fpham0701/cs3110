open Players
open Cards
open Actions
open State

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

let start_game player_names =
  let players = create_players_with_names (List.rev player_names) in
  create_state players

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
  let deck = create_deck () in
  let flop, remaining_deck = split_at 3 deck in
  set_community_cards state flop;
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "=========================================";
  Printf.printf "            Flop: %s\n"
    (String.concat ", " (List.map Cards.string_of_card flop));
  print_endline "=========================================\n";

  (* Another betting phase after the flop *)
  bet_round state;

  (* Reveal the turn (4th community card) *)
  let turn, remaining_deck = split_at 1 remaining_deck in
  set_community_cards state turn;
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "=========================================";
  Printf.printf "          Turn: %s\n"
    (String.concat ", "
       (List.map Cards.string_of_card (get_community_cards state)));
  print_endline "=========================================\n";

  (* Another betting phase after the turn *)
  bet_round state;

  (* Reveal the river (5th community card) *)
  let river, _ = split_at 1 remaining_deck in
  set_community_cards state river;
  print_endline ("\n" ^ String.make 50 '\n');
  print_endline "=========================================";
  Printf.printf "         River: %s\n"
    (String.concat ", "
       (List.map Cards.string_of_card (get_community_cards state)));
  print_endline "=========================================\n";

  (* Final betting phase after the river *)
  bet_round state;

  print_endline "Enter who the winner is: ";

  let winner = read_line () in
  Printf.printf "Congrats to %s!" winner;
  Printf.printf "You have won %d!" (get_pot state)
