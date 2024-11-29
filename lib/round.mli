open Players
open State

val start_game : string list -> State.t
(** [start_game player_names] initializes the game with the given list of player
    names and sets up the game state. *)

val game_loop : State.t -> unit
(** [game_loop state] handles the main gameplay loop, where players take turns
    in a round. The state is updated after every turn. *)
