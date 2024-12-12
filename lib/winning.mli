open Cards

type t =
  | RoyalFlush
  | StraightFlush of rank
  | FourOfAKind of rank * rank
  | FullHouse of rank * rank
  | Flush of rank list
  | Straight of rank
  | ThreeOfAKind of rank * rank list
  | TwoPair of rank * rank * rank
  | OnePair of rank * rank list
  | HighCard of rank list
      (** Abstract type that reprsents the different winning combinations in
          poker *)

val rank_value : Cards.rank -> int
(** [rank_value rank] takes in a card [rank] and converts it into a numerical
    value. *)

val best_hand_of_seven : Cards.t list -> t
(** [best_hand_of_seven cards7] takes the best 5-card hand of [cards7]. *)

val compare_hand_rank : t -> t -> int
(** [compare_hand_rank h1 h2] compares the hand rank values of [h1] and [h2] and
    determines which is better. Returns > 0 if h1 is better than h2, 0 if equal,
    < 0 if h1 is worse than h2 *)
