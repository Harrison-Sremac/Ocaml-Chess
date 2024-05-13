open Types

type game_state = {
  board : Board.board;
  turn : Types.color;
  game_over : bool;
  castling : castling_rights;
  last_move : move option;
}

val init_game : unit -> game_state

val make_move :
  game_state -> Board.position -> Board.position -> Types.color -> game_state

val check_game_status : game_state -> bool * string
