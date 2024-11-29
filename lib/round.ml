open Players
open Cards
open Actions
open State

(** [split_at n] splits the list at index [n] *)
let split_at n lst =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | x :: xs when i < n -> aux (i + 1) (x :: acc) xs
    | xs -> (List.rev acc, xs)
  in
  aux 0 [] lst

let start_game player_names =
  let players = create_players_with_names (List.rev player_names) in
  create_state players

let game_loop state =
  let active_players = get_players state in
  if List.length active_players <= 1 then
    match active_players with
    | head :: [] ->
        Printf.printf "%s is the winner!" (Players.get_name head);
        print_endline "Game over!"
    | _ -> print_endline "Game over!"
  else
    (* Betting phase before community cards *)
    List.iter
      (fun player ->
        let options = [ "call"; "check"; "fold"; "raise" ] in
        let action = action player state options in
        match action with
        | Call -> call state player (get_pot state)
        | Check -> check state player
        | Fold -> fold state player
        | Raise amount -> raise_bet state player amount)
      (get_players state);

  (* Reveal the flop (first 3 community cards) *)
  let deck = create_deck () in
  let flop, _ = split_at 3 deck in
  (* Split the deck into first 3 cards and remaining *)
  set_community_cards state flop;
  print_endline "\n=====================================";
  Printf.printf "          Flop: %s\n"
    (String.concat ", " (List.map Cards.string_of_card flop));
  print_endline "=====================================\n";

  (* Another betting phase after flop *)
  List.iter
    (fun player ->
      let options = [ "call"; "check"; "fold"; "raise" ] in
      let action = action player state options in
      match action with
      | Call -> call state player (get_pot state)
      | Check -> ()
      | Fold -> fold state player
      | Raise amount -> raise_bet state player amount)
    (get_players state)
