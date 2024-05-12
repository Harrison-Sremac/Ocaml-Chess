open Board

type piece =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

type color =
  | White
  | Black

type position = char * int (* e.g., ('e', 2) *)
type move = position * position

(* Generates a list of possible moves for a given piece at a given position *)

val possible_moves :
  piece -> color -> position -> board -> (position * position) list
