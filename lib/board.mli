open Types

type position = char * int
type board = (position * (piece * color)) list

val string_of_piece : piece -> string
val string_of_color : color -> string
val string_of_position : position -> string
val board_as_list : board -> (position * (piece * color)) list
val piece_at_position : ('a * 'b) list -> 'a -> 'b option
val create_initial_castling_rights : unit -> castling_rights
val initialize_board : unit -> board
val board_to_string : board -> string
val is_valid_move : board -> position -> position -> color -> bool
val make_move : board -> position -> position -> color -> board
val promote_pawn : board -> position -> color -> board
val check_mate : board -> color -> bool
val stale_mate : board -> color -> bool
val all_possible_moves : board -> color -> (position * position) list
val switch_turn : color -> color
val king_in_check : board -> color -> bool
val valid_moves_for_piece : board -> position -> (position * position) list
val is_checkmate : board -> color -> bool
val is_stalemate : board -> color -> bool
val move_piece : board -> position -> position -> (position * position) list
val print_board : board -> unit
