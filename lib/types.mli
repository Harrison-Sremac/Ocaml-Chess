(** Represents the different types of pieces in chess. *)
type piece =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

(** Represents the two colors of pieces in chess. *)
type color =
  | White
  | Black

type position = char * int
(** A position on the chessboard, represented as a character (file) and an
    integer (rank). *)

type board = (position * (piece * color)) list
(** The board is represented as a list of positions and associated pieces with
    their colors. *)

type castling_rights = {
  white_kingside : bool;
  white_queenside : bool;
  black_kingside : bool;
  black_queenside : bool;
}
(** Represents the castling rights for both players. *)

type move = position * position
(** Represents a move as a tuple of two positions: the start and end positions. *)
