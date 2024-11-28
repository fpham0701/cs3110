type t
(** Abstract type representing a player's action in a poker game *)

val action : string list -> t
(** [action options] prompts the user to choose an action from the available
    [options] and calls the function (call, check, fold, or raise). *)

val call : t
(** [call] represents the action of calling the current bet in the game. *)

val check : t
(** [check] represents the action of checking, meaning the player chooses not to
    bet if no other bet has been made. *)

val fold : t
(** [fold] represents the action of folding, meaning the player gives up their
    hand and forfeits the round. *)

val raise : int -> t
(** [raise amount] represents the action of raising the current bet by [amount],
    meaning the player increases the stakes by betting more. *)

val bet_size : int -> int
(** [bet_size amount] allows the user to specify the size of their bet by
    providing an entering how much they want to bet with the options twice (2x)
    the size of the bet, three times (3x), four times (4x), pot, all-in or a
    custom size. The function returns the chosen bet size. *)
