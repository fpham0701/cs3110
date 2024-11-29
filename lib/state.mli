open Players

type t
(** Abstract type representing the state of the poker game. *)

val create_state : Players.t list -> t
(** [create_state players] initializes a new game state with the given list of
    [players], an empty pot, no community cards, an empty contributions table,
    and a shuffled deck. *)

val update_pot : t -> int -> unit
(** [update_pot state amount] adds [amount] to the pot in the given game
    [state]. *)

val get_pot : t -> int
(** [get_pot state] returns the current pot value from the given game [state]. *)

val get_current_bet : t -> int
(** [get_current_bet state] returns the current bet value from the given game
    [state]. *)

val reset_current_bet : t -> unit
(** [reset_current_bet state] resets the current bet to be 0]*)

val get_players : t -> Players.t list
(** [get_players state] returns the list of active players in the current game
    [state]. *)

val get_community_cards : t -> Cards.t list
(** [get_community_cards state] returns the game [state]'s community cards. *)

val set_community_cards : t -> Cards.t list -> unit
(** [set_community_cards state flop] sets the game [state]'s community cards to
    be [flop]. *)

val call : t -> Players.t -> int -> unit
(** [call state player amount] allows the given [player] to call by matching the
    current bet with [amount]. Updates the pot and the player's contribution in
    the given game [state]. *)

val check : t -> Players.t -> unit
(** [check state player] allows the given [player] to check if their
    contribution matches the current bet in the given game [state]. Raises
    [Invalid_argument] if the contribution does not match. *)

val fold : t -> Players.t -> unit
(** [fold state player] removes the given [player] from the list of active
    players in the game [state] and updates the game accordingly. *)

val raise_bet : t -> Players.t -> int -> unit
(** [raise_bet state player amount] allows the given [player] to raise the
    current bet to [amount]. Updates the pot, the player's contribution, and the
    current bet in the game [state]. Returns the new current bet amount. *)
