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

let random_two_cards () : t * t =
  let deck = create_deck () in
  let deck_size = List.length deck in
  if deck_size < 2 then failwith "Deck has fewer than 2 cards"
  else
    let first_card_index = Random.int deck_size in
    (* uses module random to generate a random number*)
    let second_card_index =
      let rec pick_different () =
        let index = Random.int deck_size in
        if index = first_card_index then pick_different () else index
      in
      pick_different ()
    in
    let first_card = List.nth deck first_card_index in
    let second_card = List.nth deck second_card_index in
    (first_card, second_card)

let string_of_card (card : t) : string =
  let suit, rank = card in
  Printf.sprintf "%s%s"
    (match rank with
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
    | Ace -> "A")
    (match suit with
    | Spade -> "♠"
    | Heart -> "♥"
    | Clover -> "♣"
    | Club -> "♦")
