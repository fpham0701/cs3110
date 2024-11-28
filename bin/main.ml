open Poker.Actions
open Poker.Players
open Poker.Cards

(* Declare players as a mutable reference to an empty list *)
let players = ref []

(** [initialize_players] prompts for the number of players and their names, then
    creates a list of players with two cards each. *)
let initialize_players () =
  print_endline "Enter the number of players (2-9): ";
  let num_players = int_of_string (read_line ()) in
  let rec get_player_names n acc =
    if n <= 0 then List.rev acc
    else (
      print_endline
        ("\nEnter name for Player " ^ string_of_int (num_players - n + 1) ^ ": ");
      let name = read_line () in
      get_player_names (n - 1) (name :: acc))
  in
  Poker.Players.create_players_with_names (get_player_names num_players [])

(** [clear_screen] adds a large number of blank lines to hide the previous
    output. *)
let clear_screen () = print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

(** [start_game] initializes players and deals cards to each, then updates the
    global players reference. *)
let start_game () =
  print_endline "Starting the game...";
  players := initialize_players ();

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
    !players;

  print_endline "All players have received their cards."

(** [player_turn player] handles a single [player]'s turn by prompting for an
    action. *)
let rec player_turn player =
  print_endline (Poker.Players.get_name player ^ ", it's your turn!");
  let options = [ "check"; "fold"; "call"; "raise" ] in
  match Poker.Actions.action options with
  | Poker.Actions.Check ->
      print_endline (Poker.Players.get_name player ^ " chose to check.\n")
  | Poker.Actions.Fold ->
      print_endline (Poker.Players.get_name player ^ " chose to fold.\n");
<<<<<<< HEAD
      players := Poker.Players.remove_player player !players
  | Poker.Actions.Call ->
      print_endline (Poker.Players.get_name player ^ " chose to call.\n")
  | Poker.Actions.Raise amount ->
=======
      Poker.Players.remove_player player player_list
  | "bet" ->
      print_endline "Enter the amount to bet: ";
      let bet_amount = int_of_string (read_line ()) in
      (* Poker.Players.place_bet player bet_amount; *)
      print_endline (Poker.Players.get_name player ^ " chose to bet " ^ string_of_int bet_amount ^ ".\n");
      print_endline (Poker.Players.get_name player ^ " chose to bet.\n");

      player_list
  | "quit" ->
>>>>>>> 8afdb3ecb2b45eeab071cd702e4579e803d07305
      print_endline
        (Poker.Players.get_name player
        ^ " raised the bet to " ^ string_of_int amount ^ ".\n")

<<<<<<< HEAD
(** [game_loop players] iterates through all [players], giving each a turn in
    each round. *)
let rec game_loop () =
  List.iter (fun player -> player_turn player) !players;
=======
let flop deck =
  let deck = Poker.Cards.create_deck () in
  let drawn1 = Poker.Cards.draw deck in
  let drawn2 = Poker.Cards.draw (snd drawn1) in
  let drawn3 = Poker.Cards.draw (snd drawn2) in
  (fst drawn1, fst drawn2, fst drawn3, snd drawn3)

(** [game_loop player_list] iterates through the [player_list], giving each a
    turn in each round, and returns the updated list of players. *)
let rec game_loop player_list =
  let updated_players =
    List.fold_left
      (fun acc player ->
        if
          List.exists
            (fun p -> Poker.Players.get_name p = Poker.Players.get_name player)
            acc
        then player_turn player acc
        else acc)
      player_list player_list
  in
>>>>>>> 8afdb3ecb2b45eeab071cd702e4579e803d07305
  print_endline "Round completed. Moving to the next round.\n";
  game_loop ()

let main () =
  Random.self_init ();
  print_endline "\nPoker? Type 'y' or 'n'";
  let user_input = read_line () in
  match user_input with
  | "y" ->
      print_endline "\nWelcome to Poker!";
      start_game ();
      game_loop ()
  | "n" -> print_endline "Sorry to see you go."
  | _ -> print_endline "Oops, something went wrong. The input is invalid!"

let () = main ()
