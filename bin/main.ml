open Chess.Board
open Chess.Types
open Chess.Input
open Chess.Game

let print_welcome_message () =
  print_endline "Welcome to OCaml Chess!";
  print_endline
    "Instructions: Enter moves in the format 'e2 e4' to move a piece from e2 \
     to e4.";
  print_endline "Type 'quit' to quit the game."

let rec game_loop state =
  print_board state.board;
  if state.game_over then
    let _, status = check_game_status state in
    print_endline ("Game over! " ^ status)
  else (
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    match read_move () with
    | Some Quit ->
        print_endline "Exiting the game.";
        exit 0
    | Some (Move (src, dest)) ->
        let new_state = make_move state src dest state.turn in
        game_loop new_state
    | None ->
        print_endline "Invalid input. Please try again.";
        game_loop state)

let main () =
  print_welcome_message ();
  let initial_state = init_game () in
  game_loop initial_state

let () = main ()
