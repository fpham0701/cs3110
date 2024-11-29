open Players
open Cards

type t = {
  mutable players : Players.t list;
  mutable pot : int;
  mutable community_cards : Cards.t list;
  mutable current_bet : int;
}

let create_state players =
  { players; pot = 0; community_cards = []; current_bet = 0 }

let update_pot state amount = state.pot <- state.pot + amount
let get_pot state = state.pot
let get_current_bet state = state.current_bet
let reset_current_bet state = state.current_bet <- 0
let get_players state = state.players
let get_community_cards state = state.community_cards

let set_community_cards state cards =
  state.community_cards <- state.community_cards @ cards

let call state player amount =
  set_contributions player amount;
  update_pot state amount

let check state player =
  let player_contribution = get_contributions player in
  if player_contribution <> state.current_bet then
    raise
      (Invalid_argument "Player's contribution does not match the current bet")
  else
    print_endline (get_name player ^ " has checked and matched the current bet.")

let fold state player =
  state.players <-
    List.filter (fun p -> get_name p <> get_name player) state.players;
  Printf.printf "\n%s has folded!\n" (get_name player);
  if List.length state.players = 1 then
    match state.players with
    | [ winner ] ->
        Printf.printf "\n%s is the winner!\n" (get_name winner);
        print_endline "Game over!"
    | _ -> ()

let raise_bet state player amount =
  let new_bet = state.current_bet + amount in
  set_contributions player new_bet;
  update_pot state amount;
  Printf.printf "%s has raised the current bet of %i to %i!\n\n"
    (get_name player) state.current_bet new_bet;
  state.current_bet <- new_bet
