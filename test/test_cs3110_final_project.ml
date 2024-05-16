open OUnit2
open Chess.Board
open Chess.Types
open Chess.Pieces

let test_initial_board _ =
  let initial_board = initialize_board () in
  assert_equal 32
    (List.length initial_board)
    ~msg:"Initial board must contain 32 pieces"

let test_initial_positions _ =
  let board = initialize_board () in
  let piece_at pos = List.find_opt (fun (p, _) -> p = pos) board in
  let assert_piece pos expected_piece =
    match piece_at pos with
    | Some (_, piece) -> assert_equal expected_piece piece
    | None ->
        assert_failure
          ("No piece found at position "
          ^ Char.escaped (fst pos)
          ^ string_of_int (snd pos))
  in
  (* White pieces *)
  assert_piece ('a', 1) (Rook, White);
  assert_piece ('b', 1) (Knight, White);
  assert_piece ('c', 1) (Bishop, White);
  assert_piece ('d', 1) (Queen, White);
  assert_piece ('e', 1) (King, White);
  assert_piece ('f', 1) (Bishop, White);
  assert_piece ('g', 1) (Knight, White);
  assert_piece ('h', 1) (Rook, White);
  assert_piece ('a', 2) (Pawn, White);
  assert_piece ('b', 2) (Pawn, White);
  assert_piece ('c', 2) (Pawn, White);
  assert_piece ('d', 2) (Pawn, White);
  assert_piece ('e', 2) (Pawn, White);
  assert_piece ('f', 2) (Pawn, White);
  assert_piece ('g', 2) (Pawn, White);
  assert_piece ('h', 2) (Pawn, White);
  (* Black pieces *)
  assert_piece ('a', 8) (Rook, Black);
  assert_piece ('b', 8) (Knight, Black);
  assert_piece ('c', 8) (Bishop, Black);
  assert_piece ('d', 8) (Queen, Black);
  assert_piece ('e', 8) (King, Black);
  assert_piece ('f', 8) (Bishop, Black);
  assert_piece ('g', 8) (Knight, Black);
  assert_piece ('h', 8) (Rook, Black);
  assert_piece ('a', 7) (Pawn, Black);
  assert_piece ('b', 7) (Pawn, Black);
  assert_piece ('c', 7) (Pawn, Black);
  assert_piece ('d', 7) (Pawn, Black);
  assert_piece ('e', 7) (Pawn, Black);
  assert_piece ('f', 7) (Pawn, Black);
  assert_piece ('g', 7) (Pawn, Black);
  assert_piece ('h', 7) (Pawn, Black)

let test_pawn_initial_move _ =
  let board = initialize_board () in
  let valid_moves = pawn_moves White ('e', 2) board in
  let expected_moves = [ (('e', 2), ('e', 3)); (('e', 2), ('e', 4)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_pawn_capture _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('d', 7) ('d', 5) Black in
  let valid_moves = pawn_moves White ('e', 4) board in
  let expected_moves = [ (('e', 4), ('e', 5)); (('e', 4), ('d', 5)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_king_move _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 3) White in
  let board = make_move board ('a', 7) ('a', 6) Black in
  let board = make_move board ('e', 1) ('e', 2) White in
  let valid_moves = king_moves White ('e', 2) board in
  let expected_moves =
    [ (('e', 2), ('e', 1)); (('e', 2), ('d', 3)); (('e', 2), ('f', 3)) ]
  in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_knight_move _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('b', 1) board in
  let expected_moves = [ (('b', 1), ('a', 3)); (('b', 1), ('c', 3)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_rook_move _ =
  let board = initialize_board () in
  let board = make_move board ('a', 2) ('a', 4) White in
  let valid_moves = rook_moves White ('a', 1) board in
  let expected_moves = [ (('a', 1), ('a', 3)); (('a', 1), ('a', 2)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_bishop_move _ =
  let board = initialize_board () in
  let board = make_move board ('a', 2) ('a', 4) White in
  let board = make_move board ('b', 7) ('b', 6) Black in
  let board = make_move board ('b', 2) ('b', 4) White in
  let board = make_move board ('c', 7) ('c', 6) Black in
  let board = make_move board ('c', 2) ('c', 4) White in
  let board = make_move board ('d', 7) ('d', 6) Black in
  let valid_moves = bishop_moves White ('c', 1) board in
  let expected_moves = [ (('c', 1), ('b', 2)); (('c', 1), ('a', 3)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_en_passant _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let valid_moves = pawn_moves White ('d', 4) board in
  let expected_moves = [ (('d', 4), ('e', 5)); (('d', 4), ('d', 5)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_board_to_string_after_moves _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let board_str = board_to_string board in
  let expected_str =
    "+    a b c d e f g h    +\n\
     8  | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |  8\n\
     7  | ♟ ♟ ♟ ♟ . ♟ ♟ ♟ |  7\n\
     6  | . . . . . . . . |  6\n\
     5  | . . . . ♟ . . . |  5\n\
     4  | . . . . ♙ . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♙ ♙ ♙ ♙ . ♙ ♙ ♙ |  2\n\
     1  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  1\n\
     +    a b c d e f g h    +"
  in
  assert_equal expected_str board_str

let test_invalid_pawn_move _ =
  let board = initialize_board () in
  let valid_moves = pawn_moves White ('e', 2) board in
  assert_bool "Pawn should not move backward"
    (not (List.exists (fun (_, pos) -> pos = ('e', 1)) valid_moves))

let test_invalid_knight_move _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('b', 1) board in
  assert_bool "Knight should not move to a2"
    (not (List.exists (fun (_, pos) -> pos = ('a', 2)) valid_moves))

let test_invalid_queen_move _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let valid_moves = queen_moves White ('d', 1) board in
  assert_bool "Queen should not move in an L"
    (not (List.exists (fun (_, pos) -> pos = ('f', 3)) valid_moves))

let test_invalid_rook_move _ =
  let board = initialize_board () in
  let board = make_move board ('b', 2) ('b', 4) White in
  let valid_moves = rook_moves White ('a', 1) board in
  assert_bool "Rook should not move diagonally"
    (not (List.exists (fun (_, pos) -> pos = ('b', 2)) valid_moves))

let test_invalid_bishop_move _ =
  let board = initialize_board () in
  let board = make_move board ('f', 2) ('f', 4) White in
  let valid_moves = bishop_moves White ('f', 1) board in
  assert_bool "Bishio should not move forward"
    (not (List.exists (fun (_, pos) -> pos = ('f', 2)) valid_moves))

let test_invalid_king_move _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let valid_moves = king_moves White ('e', 1) board in
  assert_bool "King can't move two places"
    (not (List.exists (fun (_, pos) -> pos = ('e', 3)) valid_moves))

let stay_on_board_left _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('h', 1) board in
  assert_bool "Rook can't move out of board"
    (not (List.exists (fun (_, pos) -> pos = ('i', 1)) valid_moves))

let stay_on_board_right _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('a', 1) board in
  assert_bool "Rook can't move out of board"
    (not
       (List.exists
          (fun (_, pos) -> pos = (char_of_int (Char.code 'a' - 1), 1))
          valid_moves))

let stay_on_board_top _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('h', 1) board in
  assert_bool "Rook can't move out of board"
    (not (List.exists (fun (_, pos) -> pos = ('h', 0)) valid_moves))

let stay_on_board_bottom _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('h', 8) board in
  assert_bool "Rook can't move out of board"
    (not (List.exists (fun (_, pos) -> pos = ('h', 9)) valid_moves))

let chess_output _ =
  let real_str =
    "+    a b c d e f g h    +\n\
     8  | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |  8\n\
     7  | ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ |  7\n\
     6  | . . . . . . . . |  6\n\
     5  | . . . . . . . . |  5\n\
     4  | . . . . . . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ |  2\n\
     1  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  1\n\
     +    a b c d e f g h    +"
  in
  let board_str = board_to_string (initialize_board ()) in
  assert_equal real_str board_str

let spots_with_same_color _ =
  let board = initialize_board () in
  assert_equal
    (List.exists
       (fun (_, pos) -> pos = ('g', 1))
       (all_possible_moves board White))
    false

let test_queen _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 6) Black in
  let board = make_move board ('d', 1) ('d', 3) White in
  let board = make_move board ('d', 6) ('d', 5) Black in
  let valid_moves = queen_moves White ('d', 3) board in
  let expected_moves =
    [
      (('d', 3), ('a', 3));
      (('d', 3), ('b', 3));
      (('d', 3), ('c', 3));
      (('d', 3), ('e', 3));
      (('d', 3), ('f', 3));
      (('d', 3), ('g', 3));
      (('d', 3), ('h', 3));
      (('d', 3), ('c', 4));
      (('d', 3), ('b', 5));
      (('d', 3), ('a', 6));
      (('d', 3), ('e', 4));
      (('d', 3), ('f', 5));
      (('d', 3), ('g', 6));
      (('d', 3), ('h', 7));
      (('d', 3), ('d', 2));
      (('d', 3), ('d', 1));
    ]
  in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let switch_turnw _ = assert_equal (switch_turn White) Black
let switch_turnb _ = assert_equal (switch_turn Black) White

let pro_pawn _ =
  let board = initialize_board () in
  let board = make_move board ('b', 2) ('b', 4) White in
  let board = make_move board ('c', 7) ('c', 5) Black in
  let board = make_move board ('b', 4) ('c', 5) White in
  let board = make_move board ('b', 7) ('b', 6) Black in
  let board = make_move board ('c', 5) ('c', 6) White in
  let board = make_move board ('c', 8) ('b', 7) Black in
  let board = make_move board ('c', 6) ('c', 7) White in
  let board = make_move board ('b', 6) ('b', 5) Black in
  let board1 = make_move board ('c', 7) ('c', 8) White in
  let str =
    "+    a b c d e f g h    +\n\
     8  | ♜ ♞ ♕ ♛ ♚ ♝ ♞ ♜ |  8\n\
     7  | ♟ ♝ . ♟ ♟ ♟ ♟ ♟ |  7\n\
     6  | . . . . . . . . |  6\n\
     5  | . ♟ . . . . . . |  5\n\
     4  | . . . . . . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♙ . ♙ ♙ ♙ ♙ ♙ ♙ |  2\n\
     1  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  1\n\
     +    a b c d e f g h    +"
  in
  assert_equal str (board_to_string (promote_pawn board1 ('c', 8) White))

let initial_moves _ =
  let board = initialize_board () in
  let moves = all_possible_moves board White in
  assert_equal (List.length moves) 20

let check_checkmate_1 _ =
  let board = initialize_board () in
  assert_equal (check_mate board White) false

let check_stalemate_1 _ =
  let board = initialize_board () in
  assert_equal (stale_mate board White) false

let checkmate_2 _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('f', 7) ('f', 5) Black in
  let board = make_move board ('e', 4) ('f', 5) White in
  let board = make_move board ('g', 7) ('g', 5) Black in
  let board = make_move board ('d', 1) ('h', 5) White in
  assert_equal (check_mate board Black) true

let stalemate_2 _ =
  let board = initialize_board () in
  let board = make_move board ('c', 2) ('c', 4) White in
  let board = make_move board ('h', 7) ('h', 5) Black in
  let board = make_move board ('h', 2) ('h', 4) White in
  let board = make_move board ('a', 7) ('a', 5) Black in
  let board = make_move board ('d', 1) ('a', 4) White in
  let board = make_move board ('a', 8) ('a', 6) Black in
  let board = make_move board ('a', 4) ('a', 5) White in
  let board = make_move board ('a', 6) ('h', 6) Black in
  let board = make_move board ('a', 5) ('c', 7) White in
  let board = make_move board ('f', 7) ('f', 6) Black in
  let board = make_move board ('c', 7) ('d', 7) White in
  let board = make_move board ('e', 8) ('f', 7) Black in
  let board = make_move board ('d', 7) ('b', 7) White in
  let board = make_move board ('d', 8) ('d', 3) Black in
  let board = make_move board ('b', 7) ('b', 8) White in
  let board = make_move board ('d', 3) ('h', 7) Black in
  let board = make_move board ('b', 8) ('c', 8) White in
  let board = make_move board ('f', 7) ('g', 6) Black in
  let board = make_move board ('c', 8) ('e', 6) White in
  assert_equal (stale_mate board Black) true

let take_pawn _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let valid_moves = pawn_moves White ('d', 4) board in
  assert_equal (List.exists (fun (_, pos) -> pos = ('e', 5)) valid_moves) true

let take_pawn_board _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let board = make_move board ('d', 4) ('e', 5) Black in
  let str =
    "+    a b c d e f g h    +\n\
     8  | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |  8\n\
     7  | ♟ ♟ ♟ ♟ . ♟ ♟ ♟ |  7\n\
     6  | . . . . . . . . |  6\n\
     5  | . . . . ♙ . . . |  5\n\
     4  | . . . . . . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♙ ♙ ♙ . ♙ ♙ ♙ ♙ |  2\n\
     1  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  1\n\
     +    a b c d e f g h    +"
  in
  assert_equal str (board_to_string board)

let string_king _ = assert_equal (string_of_piece King) "King"
let string_queen _ = assert_equal (string_of_piece Queen) "Queen"
let string_Rook _ = assert_equal (string_of_piece Rook) "Rook"
let string_Bishop _ = assert_equal (string_of_piece Bishop) "Bishop"
let string_knight _ = assert_equal (string_of_piece Knight) "Knight"
let string_pawn _ = assert_equal (string_of_piece Pawn) "Pawn"
let string_white _ = assert_equal (string_of_color White) "White"
let string_black _ = assert_equal (string_of_color Black) "Black"
let test_position_string _ = assert_equal (string_of_position ('a', 1)) "a1"

let test_piece_at_position _ =
  let board = initialize_board () in
  assert_equal (piece_at_position board ('a', 1)) (Some (Rook, White))

let move_nothing _ =
  let board = initialize_board () in
  let board = make_move board ('a', 4) ('a', 5) White in
  assert_equal (board_to_string board) (board_to_string (initialize_board ()))

let is_checkmate_1 _ =
  let board = initialize_board () in
  assert_equal (is_checkmate board White) false

let is_stalemate_1 _ =
  let board = initialize_board () in
  assert_equal (is_stalemate board White) false

let is_checkmate_2 _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('f', 7) ('f', 5) Black in
  let board = make_move board ('e', 4) ('f', 5) White in
  let board = make_move board ('g', 7) ('g', 5) Black in
  let board = make_move board ('d', 1) ('h', 5) White in
  assert_equal (is_checkmate board Black) true

let is_stalemate_2 _ =
  let board = initialize_board () in
  let board = make_move board ('c', 2) ('c', 4) White in
  let board = make_move board ('h', 7) ('h', 5) Black in
  let board = make_move board ('h', 2) ('h', 4) White in
  let board = make_move board ('a', 7) ('a', 5) Black in
  let board = make_move board ('d', 1) ('a', 4) White in
  let board = make_move board ('a', 8) ('a', 6) Black in
  let board = make_move board ('a', 4) ('a', 5) White in
  let board = make_move board ('a', 6) ('h', 6) Black in
  let board = make_move board ('a', 5) ('c', 7) White in
  let board = make_move board ('f', 7) ('f', 6) Black in
  let board = make_move board ('c', 7) ('d', 7) White in
  let board = make_move board ('e', 8) ('f', 7) Black in
  let board = make_move board ('d', 7) ('b', 7) White in
  let board = make_move board ('d', 8) ('d', 3) Black in
  let board = make_move board ('b', 7) ('b', 8) White in
  let board = make_move board ('d', 3) ('h', 7) Black in
  let board = make_move board ('b', 8) ('c', 8) White in
  let board = make_move board ('f', 7) ('g', 6) Black in
  let board = make_move board ('c', 8) ('e', 6) White in
  assert_equal (is_stalemate board Black) true

let test_valid_move_board _ =
  let board = initialize_board () in
  assert_equal (is_valid_move board ('e', 2) ('e', 4) White) true

let test_board_list _ =
  let board = initialize_board () in
  assert_equal board (board_as_list board)

let test_valid_queen _ =
  let board = initialize_board () in
  let board = make_move board ('d', 2) ('d', 4) White in
  let board = make_move board ('e', 7) ('e', 6) Black in
  let board = make_move board ('d', 1) ('d', 3) White in
  let board = make_move board ('d', 6) ('d', 5) Black in
  let valid_moves = valid_moves_for_piece board ('d', 3) in
  let expected_moves =
    [
      (('d', 3), ('a', 3));
      (('d', 3), ('b', 3));
      (('d', 3), ('c', 3));
      (('d', 3), ('e', 3));
      (('d', 3), ('f', 3));
      (('d', 3), ('g', 3));
      (('d', 3), ('h', 3));
      (('d', 3), ('c', 4));
      (('d', 3), ('b', 5));
      (('d', 3), ('a', 6));
      (('d', 3), ('e', 4));
      (('d', 3), ('f', 5));
      (('d', 3), ('g', 6));
      (('d', 3), ('h', 7));
      (('d', 3), ('d', 2));
      (('d', 3), ('d', 1));
    ]
  in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let suite =
  "Chess Tests"
  >::: [
         "test_initial_board" >:: test_initial_board;
         "test_initial_positions" >:: test_initial_positions;
         "test_pawn_initial_move" >:: test_pawn_initial_move;
         "test_pawn_capture" >:: test_pawn_capture;
         "test_king_move" >:: test_king_move;
         "test_knight_move" >:: test_knight_move;
         (*"test_castling" >:: test_castling; *)
         "test_en_passant" >:: test_en_passant;
         "test_board_to_string_after_moves" >:: test_board_to_string_after_moves;
         "test_invalid_pawn_move" >:: test_invalid_pawn_move;
         "test_invalid_knight_move" >:: test_invalid_knight_move;
         "chess_output" >:: chess_output;
         "test_rook_move" >:: test_rook_move;
         "test_bishop_move" >:: test_bishop_move;
         "test_queen" >:: test_queen;
         "switch_turnb" >:: switch_turnb;
         "switch_turnw" >:: switch_turnw;
         "pro_pawn" >:: pro_pawn;
         "initial_moves" >:: initial_moves;
         "check_checkmate_1" >:: check_checkmate_1;
         "check_stalemate_1" >:: check_stalemate_1;
         "checkmate_2" >:: checkmate_2;
         "stalemate_2" >:: stalemate_2;
         "stay_on_board_left" >:: stay_on_board_left;
         "stay_on_board_bottom" >:: stay_on_board_bottom;
         "stay_on_board_right" >:: stay_on_board_right;
         "stay_on_board_top" >:: stay_on_board_top;
         "spots_with_same_color" >:: spots_with_same_color;
         "test_invalid_rook_move" >:: test_invalid_rook_move;
         "test_invalid_queen_move" >:: test_invalid_queen_move;
         "test_invalid_rook_move" >:: test_invalid_rook_move;
         "test_invalid_bishop_move" >:: test_invalid_bishop_move;
         "test_invalid_king_move" >:: test_invalid_king_move;
         "take_pawn" >:: take_pawn;
         "take_pawn_board" >:: take_pawn_board;
         "string_king" >:: string_king;
         "string_queen" >:: string_queen;
         "string_Rook" >:: string_Rook;
         "string_Bishop" >:: string_Bishop;
         "string_knight" >:: string_knight;
         "string_pawn" >:: string_pawn;
         "string_black" >:: string_black;
         "string_white" >:: string_white;
         "test_position_string" >:: test_position_string;
         "test_piece_at_position" >:: test_piece_at_position;
         "move_nothing" >:: move_nothing;
         "is_checkmate_1" >:: is_checkmate_1;
         "is_stalemate_1" >:: is_stalemate_1;
         "is_checkmate_2" >:: is_checkmate_2;
         "is_stalemate_2" >:: is_stalemate_2;
         "test_valid_move_board" >:: test_valid_move_board;
         "test_board_list" >:: test_board_list;
         "test_valid_queen" >:: test_valid_queen;
       ]

let () = run_test_tt_main suite
