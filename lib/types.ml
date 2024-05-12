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

type colored_piece = {
  piece_type : piece;
  color : color;
}

type position = char * int (* e.g., ('e', 2) *)
type board = (position * (piece * color)) list
