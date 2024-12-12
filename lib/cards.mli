type suit =
  | Spade
  | Heart
  | Clover
  | Club  (** Represents the four suits: Spade, Heart, Clover, and Diamonds *)

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
      (** Represents the ranking of the card, from Two to Ace, where Ace is the
          highest *)

type t
(** Abstract tuple representing the suit and rank of a card *)

val all_suits : unit -> suit list
(** [all_suits ()] returns a list of all possible suits. *)

val all_ranks : unit -> rank list
(** [all_ranks ()] returns a list of all possible ranks. *)

val match_rank : string -> rank
(** [match_rank rank] returns the correct [rank]. Raises [Invalid_argument] if
    input string does not match a valid rank*)

val match_suit : string -> suit
(** [match_suit suit] returns the correct [suit]. Raises [Invalid_argument] if
    input string does match a valid suit*)

val create_card : string -> string -> t
(** [create_card suit rank] creates a card of [suit] and [rank]. Raises an error
    if incorrect suit or rank is used. *)

val create_deck : unit -> t list
(** [create_deck ()] returns a list [t list] of all possible combinations of
    suits and ranks in a standard deck. *)

val random_two_cards : unit -> t * t
(** [random_two_cards ()] returns two random cards *)

val draw : t list -> t * t list
(** [draw lst] returns and removes a random card from deck lst. *)

val string_of_card : t -> string
(** [string_of_card card] returns a string representation of [card], displaying
    both the suit and rank in a readable format. *)

val rank_to_string : rank -> string
(** [rank_to_string] maps type rank to a string *)

val suit_to_symbol : suit -> string
(** [suit_to_symbol] maps type suit to a string *)

val size : t list -> int
(** [size deck] returns the length of the [deck]. *)

val get_suit : t -> suit
(** [get_suit card] returns the suit of [card]. *)

val get_rank : t -> rank
(** [get_rank card] returns the rank of [card]. *)

val print_two_card : t -> t -> string
(** [print card1 card2] returns a string of the [card1] and [card2] in visual
    format. *)

val print_three_card : t list -> string
(** [print cardlist] returns a string of the [cardlist] of size 3 in visual
    format. *)

val print_four_card : t list -> string * string
(** [print cardlist] returns a string tuple of the [cardlist] of size 4 in
    visual format. *)

val print_five_card : t list -> string * string
(** [print cardlist] returns a string tuple of the [cardlist] of size 5 in
    visual format. *)
