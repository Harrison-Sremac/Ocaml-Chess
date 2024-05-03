open Piece

type color =
  | White
  | Black

type piece_type = Rook

type piece = {
  piece_type : piece_type;
  color : color;
}

val string_of_piece : piece -> string
