open Types

type position = char * int
(** A position on the chessboard, represented as a character (file) and an
    integer (rank). *)

type board = (position * (piece * color)) list
(** The board is represented as a list of positions and associated pieces with
    their colors. *)

type move = position * position
(** A move is represented as a tuple of two positions: the start and end
    positions. *)

val within_board : char * int -> bool
(** [within_board pos] checks if the given [pos] is within the standard chess
    board limits. Returns [true] if the position is within the limits, [false]
    otherwise. *)

val king_moves : color -> position -> board -> (position * position) list
(** [king_moves color pos board] generates possible moves for a King from the
    given [pos] on the given [board] for the specified [color]. *)

val linear_moves :
  color -> position -> board -> (int * int) list -> (char * int) list
(** [linear_moves color pos board directions] generates possible linear moves
    from the given [pos] on the given [board] in the specified [directions] for
    the specified [color]. *)

val queen_moves : color -> position -> board -> (position * position) list
(** [queen_moves color pos board] generates possible moves for a Queen from the
    given [pos] on the given [board] for the specified [color]. *)

val rook_moves : color -> position -> board -> (position * position) list
(** [rook_moves color pos board] generates possible moves for a Rook from the
    given [pos] on the given [board] for the specified [color]. *)

val bishop_moves : color -> position -> board -> (position * position) list
(** [bishop_moves color pos board] generates possible moves for a Bishop from
    the given [pos] on the given [board] for the specified [color]. *)

val knight_moves : color -> position -> board -> (position * position) list
(** [knight_moves color pos board] generates possible moves for a Knight from
    the given [pos] on the given [board] for the specified [color]. *)

val pawn_moves : color -> position -> board -> (position * position) list
(** [pawn_moves color pos board] generates possible moves for a Pawn from the
    given [pos] on the given [board] for the specified [color]. *)

val possible_moves :
  piece -> color -> position -> board -> (position * position) list
(** [possible_moves piece color pos board] determines all possible moves for the
    given [piece] at the given [pos] on the given [board] for the specified
    [color]. *)
