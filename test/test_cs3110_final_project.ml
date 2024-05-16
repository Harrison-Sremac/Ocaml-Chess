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
  print_endline "king";
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

let test_castling _ =
  print_endline "castle";
  let board = initialize_board () in
  (* Clear path and move king and rook *)
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('e', 7) ('e', 5) Black in
  let board = make_move board ('g', 1) ('f', 3) White in
  let board = make_move board ('g', 8) ('f', 6) Black in
  let board = make_move board ('f', 1) ('c', 4) White in
  let board = make_move board ('f', 8) ('c', 5) Black in
  let board = make_move board ('d', 2) ('d', 3) White in
  let board = make_move board ('e', 8) ('f', 8) Black in
  let valid_moves = king_moves White ('e', 1) board in
  let expected_moves =
    [
      (('e', 1), ('e', 2));
      (('e', 1), ('d', 2));
      (('e', 1), ('f', 1));
      (('e', 1), ('g', 1));
    ]
  in
  List.iter
    (fun (src, dest) ->
      Printf.printf "Valid move: %c%d to %c%d\n" (fst src) (snd src) (fst dest)
        (snd dest))
    valid_moves;
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

let check_checkmate_false _ =
  let board = initialize_board () in
  assert_equal (check_mate board White) false

let check_stalemate_false _ =
  let board = initialize_board () in
  assert_equal (stale_mate board White) false

let checkmate_true _ =
  let board = initialize_board () in
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('f', 7) ('f', 5) Black in
  let board = make_move board ('e', 4) ('f', 5) White in
  let board = make_move board ('g', 7) ('g', 5) Black in
  let board = make_move board ('d', 1) ('h', 5) White in
  assert_equal (check_mate board Black) true

let stalemate_true _ =
  let board = initialize_board () in
  let board = make_move board ('c', 2) ('c', 4) White in
  let board = make_move board ('h', 7) ('h', 5) Black in
  let board = make_move board ('a', 7) ('a', 5) White in
  let board = make_move board ('d', 1) ('a', 4) Black in
  let board = make_move board ('a', 8) ('a', 6) White in
  let board = make_move board ('a', 4) ('a', 5) Black in
  let board = make_move board ('a', 6) ('h', 6) White in
  let board = make_move board ('a', 5) ('c', 7) Black in
  let board = make_move board ('f', 7) ('f', 6) White in
  let board = make_move board ('c', 7) ('d', 7) Black in
  let board = make_move board ('e', 8) ('f', 7) White in
  let board = make_move board ('d', 7) ('b', 7) Black in
  let board = make_move board ('d', 8) ('d', 3) White in
  let board = make_move board ('b', 7) ('b', 8) Black in
  let board = make_move board ('d', 3) ('h', 7) White in
  let board = make_move board ('b', 8) ('c', 8) Black in
  let board = make_move board ('f', 7) ('g', 6) White in
  let board = make_move board ('c', 8) ('e', 6) White in
  assert_equal (stale_mate board White) true

let suite =
  "Chess Tests"
  >::: [
         "test_initial_board" >:: test_initial_board;
         "test_initial_positions" >:: test_initial_positions;
         "test_pawn_initial_move" >:: test_pawn_initial_move;
         "test_pawn_capture" >:: test_pawn_capture;
         "test_king_move" >:: test_king_move;
         "test_knight_move" >:: test_knight_move;
         "test_castling" >:: test_castling;
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
         "check_checkmate_false" >:: check_checkmate_false;
         "check_stalemate_false" >:: check_stalemate_false;
         "checkmate_true" >:: checkmate_true;
         "stalemate_true" >:: stalemate_true;
       ]

let () = run_test_tt_main suite
