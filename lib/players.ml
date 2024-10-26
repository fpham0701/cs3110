open Cards

type t = {
  name : string;
  cards : Cards.t * Cards.t;
}

let get_name player = player.name
let get_card player = player.cards

let create_players () =
  let player_names =
    [
      "Player 1";
      "Player 2";
      "Player 3";
      "Player 4";
      "Player 5";
      "Player 6";
      "Player 7";
      "Player 8";
      "Player 9";
    ]
  in
  List.map
    (fun name ->
      let player_cards = Cards.random_two_cards () in
      { name; cards = player_cards })
    player_names

let create_players_with_names names =
  let rec aux acc = function
    | [] -> List.rev acc
    | name :: rest ->
        let card1, card2 = Cards.random_two_cards () in
        aux ({ name; cards = (card1, card2) } :: acc) rest
  in
  aux [] names

let print_player player =
  Printf.printf "Player: %s\nCards: %s\n" player.name
    (Printf.sprintf "%s, %s"
       (Cards.string_of_card (fst player.cards))
       (Cards.string_of_card (snd player.cards)))
