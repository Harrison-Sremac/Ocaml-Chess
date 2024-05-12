open Chess.Board
open Chess.Types

type state = {
  board : board;
  turn : color;
  game_over : bool;
}

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
    match read_line () with
    | "exit" ->
        print_endline "Exiting the game.";
        exit 0
    | move ->
        let move_parts = String.split_on_char ' ' move in
        if List.length move_parts <> 2 then begin
          print_endline "Invalid move format. Please try again.";
          game_loop state
        end
        else begin
          let src =
            ( (move_parts |> List.hd).[0],
              int_of_string (String.sub (List.hd move_parts) 1 1) )
          in
          let dest =
            ( (move_parts |> List.tl |> List.hd).[0],
              int_of_string (String.sub (List.hd (List.tl move_parts)) 1 1) )
          in
          if is_valid_move state.board src dest then begin
            let new_board = make_move state.board src dest in
            let next_turn = switch_turn state.turn in
            let game_over = check_mate new_board || stale_mate new_board in
            let updated_state =
              { board = new_board; turn = next_turn; game_over }
            in
            game_loop updated_state
          end
          else begin
            print_endline "Invalid move. Please try again.";
            game_loop state
          end
        end
  end

let main () =
  print_welcome_message ();
  let initial_state =
    { board = initialize_board (); turn = White; game_over = false }
  in
  game_loop initial_state

let () = main ()
