type t =
  | Call
  | Check
  | Fold
  | Raise of int
      (** Abstract type representing a player's action in a poker game *)

val action : Players.t -> State.t -> string list -> t
(** [action options] prompts the user to choose an action for [player] for the
    current game [state] from the available [options] and calls the function
    (call, check, fold, or raise). *)
