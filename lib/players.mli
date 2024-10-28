type t
(** Abstract type representing a player and their cards *)

val get_name : t -> string
(** [get_name player] returns the name of [player]. *)

val get_card : t -> Cards.t * Cards.t
(** [get_name card] returns the card of [player]. *)

val create_players : unit -> t list
(** [create_players ()] creates a list of default players with names "Player 1",
    "Player 2", "Player 3", and "Player 4", each with two random cards. *)

val create_players_with_names : string list -> t list
(** [create_players_with_names names] creates a list of players with the given
    [names], each player receiving two random cards. *)

val print_player : t -> unit
(** [print_player player] prints the name and cards of [player] in a readable
    format. *)

val remove_player : t -> t list -> t list
(** [remove_player player player_list] removes the given [player] from the
    [player_list]. *)
