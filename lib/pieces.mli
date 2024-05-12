open Types

type position = char * int
type board = (position * (piece * color)) list
type move = position * position

(* Helper function to check if a position is within the standard chess board
   limits *)
val within_board : char * int -> bool

(* Generates possible moves for a King from a given position on a given board *)
val king_moves : color -> position -> board -> (position * position) list

(* Generates possible linear moves from a given position on a given board in
   specified directions *)
val linear_moves :
  color -> position -> board -> (int * int) list -> (char * int) list

(* Generates possible moves for a Queen from a given position on a given
   board *)
val queen_moves : color -> position -> board -> (position * position) list

(* Generates possible moves for a Rook from a given position on a given board *)
val rook_moves : color -> position -> board -> (position * position) list

(* Generates possible moves for a Bishop from a given position on a given
   board *)
val bishop_moves : color -> position -> board -> (position * position) list

(* Generates possible moves for a Knight from a given position on a given
   board *)
val knight_moves : color -> position -> board -> (position * position) list

(* Generates possible moves for a Pawn from a given position on a given board *)
val pawn_moves : color -> position -> board -> (position * position) list

(* Determines all possible moves for a given piece at a given position on a
   given board *)
val possible_moves :
  piece -> color -> position -> board -> (position * position) list
