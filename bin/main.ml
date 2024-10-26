open Poker.Actions
open Poker.Players
open Poker.Cards
(* open Poker.OnTable *)

(** [initialize_players] prompts for the number of players and their names, then
    creates a list of players with two cards each. *)
let initialize_players () =
  print_endline "Enter the number of players (2-9): ";
  let num_players = int_of_string (read_line ()) in
  let rec get_player_names n acc =
    if n <= 0 then List.rev acc
    else (
      print_endline
        ("Enter name for Player " ^ string_of_int (num_players - n + 1) ^ ": ");
      let name = read_line () in
      get_player_names (n - 1) (name :: acc))
  in
  Poker.Players.create_players_with_names (get_player_names num_players [])

(** [clear_screen] adds a large number of blank lines to hide the previous
    output. *)
let clear_screen () = print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

(** [start_game] initializes players and deals cards to each, then returns the
    list of players. *)
let start_game () =
  print_endline "Starting the game...";
  let players = initialize_players () in

  print_endline "Press Enter to begin revealing each player's cards.";
  ignore (read_line ());

  List.iter
    (fun player ->
      print_endline
        ("Press Enter to reveal " ^ Poker.Players.get_name player ^ "'s cards.");
      ignore (read_line ());
      print_endline ("Dealing cards to " ^ Poker.Players.get_name player ^ "...");
      Poker.Players.print_player player;

      print_endline "Press Enter to continue.";
      ignore (read_line ());
      clear_screen ())
    players;

  print_endline "All players have received their cards.";
  players

(** [player_turn player] handles a single player's turn by prompting for an
    action. *)
let rec player_turn player =
  print_endline (Poker.Players.get_name player ^ ", it's your turn!");
  print_endline "Choose an action: 'check', 'fold', 'bet', or 'quit'";
  let action = read_line () in
  match action with
  | "check" ->
      print_endline (Poker.Players.get_name player ^ " chose to check.");
      ()
  | "fold" ->
      print_endline (Poker.Players.get_name player ^ " chose to fold.");
      ()
  | "bet" ->
      print_endline (Poker.Players.get_name player ^ " chose to bet.");
      ()
  | "quit" -> print_endline "Exiting the game. Thank you for playing!"
  | _ ->
      print_endline "Invalid action, please try again.";
      player_turn player

(** [game_loop players] iterates through all players, giving each a turn in each
    round. *)
let rec game_loop players =
  List.iter (fun player -> player_turn player) players;
  print_endline "Round completed. Moving to the next round.";
  game_loop players

let main () =
  Random.self_init ();

  print_endline "Poker? Type 'y' or 'n'";
  let user_input = read_line () in
  match user_input with
  | "y" ->
      print_endline "Welcome!";
      let players = start_game () in
      game_loop players
  | "n" -> print_endline "Sorry to see you go."
  | _ -> print_endline "Oops, something went wrong. The input is invalid!"

let () = main ()
