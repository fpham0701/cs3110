open Poker.Round
open Poker.Cards

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
        ("Enter the name of Player "
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
        let card1, card2 = player.cards in
        Printf.printf "Cards: %s, %s\n"
          (Poker.Cards.string_of_card card1)
          (Poker.Cards.string_of_card card2);
        print_endline "\nPress Enter to continue.";
        ignore (read_line ());
        (* ignores user's input*)
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
      let state = Poker.Round.start_game player_names in
      Printf.printf "\nPress Enter to see %s's Cards"
        (Poker.Players.get_name (List.hd state.players));
      ignore (read_line ());
      reveal_player_cards state.players;
      Poker.Round.game_loop state
  | "n" | "N" ->
      print_endline "Sorry to see you go.";
      ()
  | _ ->
      print_endline "Invalid input. Please restart the game.";
      main ()

let () = main ()
