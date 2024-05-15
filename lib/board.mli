open Types

type position = char * int
type board = (position * (piece * color)) list

val create_initial_castling_rights : unit -> castling_rights
val initialize_board : unit -> board
val print_board : board -> unit
val board_to_string : board -> string
val is_valid_move : board -> position -> position -> color -> bool
val make_move : board -> position -> position -> color -> board
val promote_pawn : board -> position -> color -> board
val check_mate : board -> color -> bool
val stale_mate : board -> color -> bool
val all_possible_moves : board -> color -> (position * position) list
val switch_turn : color -> color
val king_in_check : board -> color -> bool
