(* Type definitions *)
open Types

type position = char * int (* e.g., ('e', 2) *)
type board = (position * (piece * color)) list

(* Initializes the chessboard with pieces at starting positions *)
val initialize_board : unit -> board

(* Prints the board to the terminal *)
val print_board : board -> unit

(* Converts a board state into a string for printing or other purposes *)
val board_to_string : board -> string

(* Checks if a move from a source to a destination is valid *)
val is_valid_move : board -> position -> position -> color -> bool

(* Moves a piece from source to destination, returning the new board state *)
val make_move : board -> position -> position -> color -> board

(* Determines if the current board is in a checkmate state *)
val check_mate : board -> bool

(* Determines if the current board is in a stalemate state *)
val stale_mate : board -> bool

(* Generates all possible moves for a given color *)
val all_possible_moves : board -> color -> (position * position) list

(* Switches the current player's turn *)
val switch_turn : color -> color
