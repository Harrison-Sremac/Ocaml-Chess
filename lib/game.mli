(* @author Ajay Tadinada (at663), Harrison Sremac (hcs59), Mericel Tao (mst223),
   Sanya Kohli (sk2682) *)

open Types

type game_state = {
  board : Board.board;
  turn : Types.color;
  game_over : bool;
  castling : castling_rights;
  last_move : move option;
}
(** The state of the game, including the board, turn, game over status, castling
    rights, and the last move. *)

val init_game : unit -> game_state
(** [init_game ()] initializes the game state with the starting board
    configuration and castling rights. *)

val make_move :
  game_state ->
  Board.position ->
  Board.position ->
  Types.color ->
  game_state * string
(** [make_move state src dest color] attempts to make a move from [src] to
    [dest] for the given [color] in the provided [state]. Returns the new game
    state and a status message. *)

val check_game_status : game_state -> bool * string
(** [check_game_status state] checks the status of the game in the given
    [state], returns a tuple of a boolean indicating if the game is over and a
    string message. *)
