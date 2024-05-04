open Chess.Board
open Chess.Input
open Chess.Ai

let print_welcome_message () =
  print_endline "Welcome to OCaml Chess!";
  print_endline
    "Instructions: Enter moves in the format 'e2 e4' to move a piece from e2 \
     to e4.";
  print_endline "Type 'exit' to quit the game."

let rec game_loop state =
  print_board state.board;
  if state.game_over then begin
    print_endline "Game over!";
    match (check_mate state.board, stale_mate state.board) with
    | true, _ -> print_endline "Checkmate!"
    | _, true -> print_endline "Stalemate!"
    | _ -> print_endline "Draw or resignation."
  end
  else begin
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    let move = Input.read_move () in
    match move with
    | Some (src, dest) ->
        let new_board = make_move state.board src dest in
        let next_turn = switch_turn state.turn in
        let game_over = check_mate new_board || stale_mate new_board in
        game_loop { board = new_board; turn = next_turn; game_over }
    | None ->
        print_endline "Invalid move or format. Please try again.";
        game_loop state
  end

let main () =
  print_welcome_message ();
  let initial_board = initialize_board () in
  let initial_state =
    { board = initial_board; turn = White; game_over = false }
  in
  game_loop initial_state

let () = main ()
