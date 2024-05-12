type game_state = {
  board : Board.board;
  turn : Types.color;
  game_over : bool;
}

(* Initialize a new game with the starting positions *)
let init_game () =
  { board = Board.initialize_board (); turn = Types.White; game_over = false }

(* Switch turns *)
let switch_turn turn =
  match turn with
  | Types.White -> Types.Black
  | Types.Black -> Types.White

(* Make a move on the board, assuming it's a legal move *)
let make_move state src dest =
  if Board.is_valid_move state.board src dest then
    let new_board = Board.make_move state.board src dest in
    {
      board = new_board;
      turn = switch_turn state.turn;
      game_over = Board.check_mate new_board || Board.stale_mate new_board;
    }
  else state

(* Check if the game is in checkmate or stalemate *)
let check_game_status state =
  if Board.check_mate state.board then (true, "Checkmate")
  else if Board.stale_mate state.board then (true, "Stalemate")
  else (false, "")
