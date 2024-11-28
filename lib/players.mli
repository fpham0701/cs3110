open Cards

type t = {
  name : string;
  mutable cards : Cards.t * Cards.t;
  mutable contribution : int;
}
(** Abstract type representing a player and their cards *)

val get_name : t -> string
(** [get_name player] returns the name of [player]. *)

val get_card : t -> Cards.t * Cards.t
(** [get_card player] returns the cards of [player]. *)

val create_players_with_names : string list -> t list
(** [create_players_with_names names] creates a list of players with the given
    [names], each player receiving two random cards. *)

val print_player : t -> unit
(** [print_player player] prints the name and cards of [player]. *)
