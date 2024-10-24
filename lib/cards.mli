type suit
(** Represents the four suits: Spade, Heart, Clover, and Diamonds *)

type rank
(** Represents the ranking of the card, from Two to Ace, where Ace is the
    highest *)

type t
(** Abstract Tuple representing the suit and rank of the card *)

val deck : t list -> t
(** [deck] contains all possible combinations of cards in a standard deck. *)

val random_two_cards : t
(** [random_two_cards] draws two different cards for players *)
