open Poker.Actions

let main () =
  print_endline "Poker? Type 'yes' or 'no'";

  let user_input = read_line () in
  match user_input with
  | "yes" -> print_endline "Welcome!"
  | "no" -> print_endline "Sorry to see you go."
  | _ -> print_endline "Oops, something went wrong. The input is invalid!"

let () = main ()
