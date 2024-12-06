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

val size : t list -> int
(** [size deck] returns the length of the [deck]. *)

val get_suit : t -> suit
(** [get_suit card] returns the suit of [card]. *)

val get_rank : t -> rank
(** [get_rank card] returns the rank of [card]. *)

val print_two_card : t -> t -> unit
(** [print card1 card2] prints the [card1] and [card2] in visual format. *)

val print_three_card : t list -> unit
(** [print cardlist] prints the [cardlist] of size 3 in visual format. *)

val print_four_card : t list -> unit
(** [print cardlist] prints the [cardlist] of size 4 in visual format. *)

val print_five_card : t list -> unit
(** [print cardlist] prints the [cardlist] of size 5 in visual format. *)

(* val best_hand : t list -> t list * [best_hand cardlist] returns the best hand
   from the [cardlist]. *)
