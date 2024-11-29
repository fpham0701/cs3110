type suit
(** Represents the four suits: Spade, Heart, Clover, and Diamonds *)

type rank
(** Represents the ranking of the card, from Two to Ace, where Ace is the
    highest *)

type t
(** Abstract tuple representing the suit and rank of a card *)

val all_suits : unit -> suit list
(** [all_suits ()] returns a list of all possible suits. *)

val all_ranks : unit -> rank list
(** [all_ranks ()] returns a list of all possible ranks. *)

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
