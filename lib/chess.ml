(* Chess.ml *)
open Board

let init_game () =
  let board = init_board () in
  setup_board board;
  board

let start_game board =
  (* Game logic to play chess, handle user input, etc. *)
  print_board board
(* Additional game loop logic here *)
