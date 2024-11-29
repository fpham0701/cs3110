open Cards

type t = {
  name : string;
  mutable cards : Cards.t * Cards.t;
  mutable contribution : int;
}

let get_name player = player.name
let get_card player = player.cards
let get_contributions player = player.contribution
let set_contributions player amount = player.contribution <- amount

let create_players_with_names names =
  let deck = create_deck () in
  let rec create_players names =
    match names with
    | [] -> []
    | name :: rest ->
        let card1, deck = draw deck in
        let card2, deck = draw deck in
        let cards = (card1, card2) in
        { name; cards; contribution = 0 } :: create_players rest
  in
  (create_players names, deck)

let print_player player =
  Printf.printf "Player: %s\nCards: %s\n" player.name
    (Cards.string_of_card (fst player.cards)
    ^ ", "
    ^ Cards.string_of_card (snd player.cards))
