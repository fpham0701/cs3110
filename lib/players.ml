open Cards

type t = {
  name : string;
  mutable cards : Cards.t * Cards.t;
  mutable contribution : int;
}

let get_name player = player.name
let get_card player = player.cards
let get_contribution player = player.contribution

let create_players_with_names names =
  let rec create_players names =
    match names with
    | [] -> []
    | name :: rest ->
        let cards = Cards.random_two_cards () in
        { name; cards; contribution = 0 } :: create_players rest
  in
  create_players names

let print_player player =
  Printf.printf "Player: %s\nCards: %s\n" player.name
    (Cards.string_of_card (fst player.cards)
    ^ ", "
    ^ Cards.string_of_card (snd player.cards))
