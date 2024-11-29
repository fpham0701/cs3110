type suit =
  | Spade
  | Heart
  | Clover
  | Club

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type t = suit * rank

let all_suits () = [ Spade; Heart; Clover; Club ]

let all_ranks () =
  [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;
  ]

let create_deck () : t list =
  List.flatten
    (List.map
       (fun suit -> List.map (fun rank -> (suit, rank)) (all_ranks ()))
       (all_suits ()))

let random_two_cards () =
  let deck = create_deck () in
  let deck_size = List.length deck in
  let first_card_index = Random.int deck_size in
  let second_card_index = Random.int (deck_size - 1) in
  let second_card_index =
    if second_card_index >= first_card_index then second_card_index + 1
    else second_card_index
  in
  (List.nth deck first_card_index, List.nth deck second_card_index)

let draw deck : t * t list =
  let deck_size = List.length deck in
  if deck_size = 0 then failwith "Deck is empty"
  else
    let card_index = Random.int deck_size in
    let card = List.nth deck card_index in
    let new_deck = List.filteri (fun i _ -> i <> card_index) deck in
    (card, new_deck)

let string_of_card (card : t) : string =
  let suit_to_string = function
    | Spade -> "♠"
    | Heart -> "♥"
    | Clover -> "♣"
    | Club -> "♦"
  in
  let rank_to_string = function
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  in
  let suit, rank = card in
  Printf.sprintf "%s%s" (rank_to_string rank) (suit_to_string suit)

let size (deck : t list) : int = List.length deck
