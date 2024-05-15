open OUnit2
open Chess.Board

let test_initial_board _ =
  let initial_board = initialize_board () in
  assert_equal 32
    (List.length initial_board)
    ~msg:"Initial board must contain 32 pieces"

let chess_output _ =
  let real_str =
    "+    a b c d e f g h    +\n\
     8  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  8\n\
     7  | ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ |  7\n\
     6  | . . . . . . . . |  6\n\
     5  | . . . . . . . . |  5\n\
     4  | . . . . . . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ |  2\n\
     1  | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |  1\n\
     +    a b c d e f g h    +"
  in
  assert_equal real_str (board_to_string (initialize_board ()))

let suite =
  "Chess Tests"
  >::: [
         "test_initial_board" >:: test_initial_board;
         "chess_output" >:: chess_output;
       ]

let () = run_test_tt_main suite
