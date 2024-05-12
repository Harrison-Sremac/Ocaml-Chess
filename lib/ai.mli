(* Represents the difficulty level or depth of the AI's search *)
type difficulty = int

(* Evaluates the board and returns the best move for the AI *)
val best_move :
  Board.board -> Types.color -> difficulty -> Board.position * Board.position
