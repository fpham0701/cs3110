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

let size (deck : t list) : int = List.length deck

let draw deck : t * t list =
  let deck_size = size deck in
  if deck_size = 0 then failwith "Deck is empty"
  else
    let card_index = Random.int deck_size in
    let card = List.nth deck card_index in
    let new_deck = List.filteri (fun i _ -> i <> card_index) deck in
    (card, new_deck)

let string_of_card (card : t) : string =
  let suit_to_string = function
    | Spade -> "\xe2\x99\xa0" (* ♠ *)
    | Heart -> "\xe2\x99\xa5" (* ♥ *)
    | Clover -> "\xe2\x99\xa3" (* ♣ *)
    | Club -> "\xe2\x99\xa6" (* ♦ *)
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

let suit_to_symbol = function
  | Spade -> "♠"
  | Heart -> "♥"
  | Clover -> "♣"
  | Club -> "♦"

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

let get_suit (card : t) : suit = fst card
let get_rank (card : t) : rank = snd card

let print_two_card (card1 : t) (card2 : t) =
  let suit1, rank1 = card1 in
  let suit_symbol1 = suit_to_symbol suit1 in
  let rank_str1 = rank_to_string rank1 in
  let suit2, rank2 = card2 in
  let suit_symbol2 = suit_to_symbol suit2 in
  let rank_str2 = rank_to_string rank2 in
  Printf.printf
    "\n\
     ┌─────────┐      ┌─────────┐\n\
     │ %-2s      │      │ %-2s      │\n\
     │         │      │         │\n\
     │    %s    │      │    %s    │\n\
     │         │      │         │\n\
     │      %-2s │      │      %-2s │\n\
     └─────────┘      └─────────┘\n"
    rank_str1 rank_str2 suit_symbol1 suit_symbol2 rank_str1 rank_str2

let print_three_card cardlist =
  if size cardlist = 3 then
    let card1 = List.nth cardlist 0 in
    let card2 = List.nth cardlist 1 in
    let card3 = List.nth cardlist 2 in

    let suit1, rank1 = card1 in
    let suit_symbol1 = suit_to_symbol suit1 in
    let rank_str1 = rank_to_string rank1 in
    let suit2, rank2 = card2 in
    let suit_symbol2 = suit_to_symbol suit2 in
    let rank_str2 = rank_to_string rank2 in
    let suit3, rank3 = card3 in
    let suit_symbol3 = suit_to_symbol suit3 in
    let rank_str3 = rank_to_string rank3 in
    Printf.printf
      "\n\
       ┌─────────┐      ┌─────────┐      ┌─────────┐\n\
       │ %-2s      │      │ %-2s      │      │ %-2s      │\n\
       │         │      │         │      │         │\n\
       │    %s    │      │    %s    │      │    %s    │\n\
       │         │      │         │      │         │\n\
       │      %-2s │      │      %-2s │      │      %-2s │\n\
       └─────────┘      └─────────┘      └─────────┘\n"
      rank_str1 rank_str2 rank_str3 suit_symbol1 suit_symbol2 suit_symbol3
      rank_str1 rank_str2 rank_str3

let print_four_card cardlist =
  if size cardlist = 4 then (
    let card1 = List.nth cardlist 0 in
    let card2 = List.nth cardlist 1 in
    let card3 = List.nth cardlist 2 in
    let card4 = List.nth cardlist 3 in

    let suit1, rank1 = card1 in
    let suit_symbol1 = suit_to_symbol suit1 in
    let rank_str1 = rank_to_string rank1 in
    let suit2, rank2 = card2 in
    let suit_symbol2 = suit_to_symbol suit2 in
    let rank_str2 = rank_to_string rank2 in
    let suit3, rank3 = card3 in
    let suit_symbol3 = suit_to_symbol suit3 in
    let rank_str3 = rank_to_string rank3 in
    let suit4, rank4 = card4 in
    let suit_symbol4 = suit_to_symbol suit4 in
    let rank_str4 = rank_to_string rank4 in
    Printf.printf
      "\n\
       |      ┌─────────┐      ┌─────────┐\n\
       |      │ %-2s      │      │ %-2s      │\n\
       |      │         │      │         │\n\
       |      │    %s    │      │    %s    │\n\
       |      │         │      │         │\n\
       |      │      %-2s │      │      %-2s │\n\
       |      └─────────┘      └─────────┘\n"
      rank_str1 rank_str2 suit_symbol1 suit_symbol2 rank_str1 rank_str2;

    Printf.printf
      "\n\
       |           ┌─────────┐      ┌─────────┐\n\
       |           │ %-2s      │      │ %-2s      │\n\
       |           │         │      │         │\n\
       |           │     %s   │      │    %s    │\n\
       |           │         │      │         │\n\
       |           │      %-2s │      │      %-2s │\n\
       |           └─────────┘      └─────────┘\n"
      rank_str3 rank_str4 suit_symbol3 suit_symbol4 rank_str3 rank_str4)

let print_five_card cardlist =
  if size cardlist = 5 then (
    let card1 = List.nth cardlist 0 in
    let card2 = List.nth cardlist 1 in
    let card3 = List.nth cardlist 2 in
    let card4 = List.nth cardlist 3 in
    let card5 = List.nth cardlist 4 in

    let suit1, rank1 = card1 in
    let suit_symbol1 = suit_to_symbol suit1 in
    let rank_str1 = rank_to_string rank1 in
    let suit2, rank2 = card2 in
    let suit_symbol2 = suit_to_symbol suit2 in
    let rank_str2 = rank_to_string rank2 in
    let suit3, rank3 = card3 in
    let suit_symbol3 = suit_to_symbol suit3 in
    let rank_str3 = rank_to_string rank3 in
    let suit4, rank4 = card4 in
    let suit_symbol4 = suit_to_symbol suit4 in
    let rank_str4 = rank_to_string rank4 in
    let suit5, rank5 = card5 in
    let suit_symbol5 = suit_to_symbol suit5 in
    let rank_str5 = rank_to_string rank5 in
    Printf.printf
      "\n\
       |   ┌─────────┐      ┌─────────┐      ┌─────────┐\n\
       |   │ %-2s      │      │ %-2s      │      │ %-2s      │\n\
       |   │         │      │         │      │         │\n\
       |   │    %s    │      │    %s    │      │    %s    │\n\
       |   │         │      │         │      │         │\n\
       |   │      %-2s │      │      %-2s │      │      %-2s │\n\
       |   └─────────┘      └─────────┘      └─────────┘\n"
      rank_str1 rank_str2 rank_str3 suit_symbol1 suit_symbol2 suit_symbol3
      rank_str1 rank_str2 rank_str3;
    Printf.printf
      "\n\
       |            ┌─────────┐      ┌─────────┐\n\
       |            │ %-2s      │      │ %-2s      │\n\
       |            │         │      │         │\n\
       |            │    %s    │      │    %s    │\n\
       |            │         │      │         │\n\
       |            │      %-2s │      │      %-2s │\n\
       |            └─────────┘      └─────────┘\n"
      rank_str4 rank_str5 suit_symbol4 suit_symbol5 rank_str4 rank_str5)
