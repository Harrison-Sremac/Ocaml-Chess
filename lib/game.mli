type game_state = {
  board : Board.board;
  turn : Types.color;
  game_over : bool;
}

(* Initialize a new game *)
val init_game : unit -> game_state

(* Attempt to move a piece from one position to another, returns updated game
   state *)
val make_move :
  game_state -> Board.position -> Board.position -> Types.color -> game_state

(* Determines if the game is in checkmate or stalemate *)
val check_game_status : game_state -> bool * string
