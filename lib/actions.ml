open Players
open State

type t =
  | Call
  | Check
  | Fold
  | Raise of int

let action player state options =
  print_endline "\n===--------------------------------===";
  Printf.printf "           Current Pot: %i           \n" (get_pot state);
  print_endline "===--------------------------------===";

  Printf.printf "\nIt is %s's turn to go!\n" (get_name player);
  print_endline ("Available actions: " ^ String.concat ", " options);
  print_endline "Enter your action:";
  let rec prompt_action () =
    let input = read_line () in
    match input with
    | "call" when List.mem "call" options -> Call
    | "check" when List.mem "check" options -> Check
    | "fold" when List.mem "fold" options -> Fold
    | "raise" when List.mem "raise" options ->
        print_endline "Enter the amount to raise:";
        let amount = int_of_string (read_line ()) in
        Raise amount
    | _ ->
        print_endline "Invalid action. Please try again.";
        prompt_action ()
  in
  prompt_action ()
