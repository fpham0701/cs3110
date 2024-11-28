type t =
  | Call
  | Check
  | Fold
  | Raise of int
      (** Abstract type representing a player's action in a poker game. *)

(** Represents the action of calling the current bet in the game. *)
let call = Call

(** Represents the action of checking, meaning the player chooses not to bet if
    no other bet has been made. *)
let check = Check

(** Represents the action of folding, meaning the player gives up their hand and
    forfeits the round. *)
let fold = Fold

(** Represents the action of raising, where a player increases the stakes by
    betting more. *)
let raise amount = Raise amount

(** Prompts the user to choose an action from the available options (call,
    check, fold, or raise). *)
let action options =
  let rec get_choice () =
    print_endline "Choose an action:";
    List.iteri (fun i option -> Printf.printf "%d. %s\n" (i + 1) option) options;
    match read_line () with
    | input -> (
        try
          let choice = int_of_string input in
          if choice > 0 && choice <= List.length options then (
            match List.nth options (choice - 1) with
            | "call" -> call
            | "check" -> check
            | "fold" -> fold
            | "raise" ->
                print_endline "Enter the amount to raise:";
                let amount = int_of_string (read_line ()) in
                raise amount
            | _ ->
                print_endline "Invalid choice. Please try again.";
                get_choice ())
          else (
            print_endline "Invalid choice. Please try again.";
            get_choice ())
        with Failure _ ->
          print_endline "Invalid input. Please enter a number.";
          get_choice ())
  in
  get_choice ()

(** Allows the user to specify the size of their bet by choosing from preset
    options or entering a custom size. Returns the chosen bet size. *)
let bet_size current_bet =
  let rec choose_size () =
    print_endline "Choose the bet size:";
    print_endline
      "1. Twice (2x) the current bet\n\
       2. Three times (3x) the current bet\n\
       3. Four times (4x) the current bet\n\
       4. Pot size\n\
       5. All-in\n\
       6. Custom size";
    match read_line () with
    | "1" -> current_bet * 2
    | "2" -> current_bet * 3
    | "3" -> current_bet * 4
    | "4" ->
        print_endline "Enter the pot size:";
        int_of_string (read_line ())
    | "5" ->
        print_endline "Enter the all-in amount:";
        int_of_string (read_line ())
    | "6" ->
        print_endline "Enter your custom bet size:";
        int_of_string (read_line ())
    | _ ->
        print_endline "Invalid choice. Please try again.";
        choose_size ()
  in
  choose_size ()
