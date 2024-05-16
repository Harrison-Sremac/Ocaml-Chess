(* @author Ajay Tadinada (at663), Harrison Sremac (hcs59), Mericel Tao (mst223),
   Sanya Kohli (sk2682) *)

open Types

type position = char * int
(** A position on the chessboard, represented as a character (file) and an
    integer (rank). *)

type board = (position * (piece * color)) list
(** The board is represented as a list of positions and associated pieces with
    their colors. *)

val string_of_piece : piece -> string
(** [string_of_piece piece] converts a piece to its string representation. *)

val string_of_color : color -> string
(** [string_of_color color] converts a color to its string representation. *)

val string_of_position : position -> string
(** [string_of_position position] converts a position to its string
    representation in the format "e2". *)

val board_as_list : board -> (position * (piece * color)) list
(** [board_as_list board] returns the board as a list of position and
    piece-color pairs. *)

val piece_at_position : ('a * 'b) list -> 'a -> 'b option
(** [piece_at_position board position] returns the piece and color at the given
    position on the board, or [None] if the position is empty. *)

val create_initial_castling_rights : unit -> castling_rights
(** [create_initial_castling_rights ()] initializes the castling rights for both
    players. *)

val initialize_board : unit -> board
(** [initialize_board ()] sets up the initial chessboard configuration. *)

val board_to_string : board -> string
(** [board_to_string board] converts the board to a string representation for
    display purposes. *)

val is_valid_move : board -> position -> position -> color -> bool
(** [is_valid_move board src dest color] checks if moving a piece from [src] to
    [dest] is a valid move for the given [color]. *)

val make_move : board -> position -> position -> color -> board
(** [make_move board src dest color] performs a move from [src] to [dest] for
    the given [color] and returns the new board state. *)

val promote_pawn : board -> position -> color -> board
(** [promote_pawn board position color] promotes the pawn at [position] to
    another piece chosen by the player, for the given [color]. *)

val check_mate : board -> color -> bool
(** [check_mate board color] checks if the given [color] is in checkmate on the
    board. *)

val stale_mate : board -> color -> bool
(** [stale_mate board color] checks if the given [color] is in stalemate on the
    board. *)

val all_possible_moves : board -> color -> (position * position) list
(** [all_possible_moves board color] returns all possible moves for the given
    [color] on the board. *)

val switch_turn : color -> color
(** [switch_turn color] switches the turn from one player to the other. *)

val king_in_check : board -> color -> bool
(** [king_in_check board color] checks if the king of the given [color] is in
    check on the board. *)

val valid_moves_for_piece : board -> position -> (position * position) list
(** [valid_moves_for_piece board position] returns all valid moves for the piece
    at the given [position] on the board. *)

val is_checkmate : board -> color -> bool
(** [is_checkmate board color] checks if the given [color] is in checkmate on
    the board. *)

val is_stalemate : board -> color -> bool
(** [is_stalemate board color] checks if the given [color] is in stalemate on
    the board. *)

val move_piece : board -> position -> position -> board
(** [move_piece board src dest] returns a list of possible moves from [src] to
    [dest] on the board. *)

val print_board : board -> unit
(** [print_board board] prints the current board state to the console. *)
