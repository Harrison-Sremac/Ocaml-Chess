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
  let board = make_move board ('e', 1) ('e', 2) White in
  let valid_moves = king_moves White ('e', 2) board in
  let expected_moves =
    [
      (('e', 2), ('d', 1));
      (('e', 2), ('d', 2));
      (('e', 2), ('e', 1));
      (('e', 2), ('e', 3));
      (('e', 2), ('f', 1));
      (('e', 2), ('f', 2));
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

let test_knight_move _ =
  let board = initialize_board () in
  let valid_moves = knight_moves White ('b', 1) board in
  let expected_moves = [ (('b', 1), ('a', 3)); (('b', 1), ('c', 3)) ] in
  assert_equal
    (List.sort compare valid_moves)
    (List.sort compare expected_moves)

let test_castling _ =
  let board = initialize_board () in
  (* Clear path and move king and rook *)
  let board = make_move board ('e', 2) ('e', 3) White in
  let board = make_move board ('e', 1) ('e', 2) White in
  let board = make_move board ('g', 1) ('g', 2) White in
  let board = make_move board ('f', 1) ('g', 1) White in
  let valid_moves = king_moves White ('e', 2) board in
  let expected_moves =
    [
      (('e', 2), ('d', 1));
      (('e', 2), ('d', 2));
      (('e', 2), ('e', 1));
      (('e', 2), ('e', 3));
      (('e', 2), ('f', 1));
      (('e', 2), ('f', 2));
      (('e', 2), ('c', 2));
      (* Castling move *)
      (('e', 2), ('g', 2));
      (* Castling move *)
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
  let board = make_move board ('e', 2) ('e', 4) White in
  let board = make_move board ('d', 7) ('d', 5) Black in
  let board = make_move board ('e', 4) ('e', 5) White in
  let board = make_move board ('d', 5) ('d', 4) Black in
  let valid_moves = pawn_moves White ('e', 5) board in
  let expected_moves = [ (('e', 5), ('e', 6)); (('e', 5), ('d', 6)) ] in
  List.iter
    (fun (src, dest) ->
      Printf.printf "Valid move: %c%d to %c%d\n" (fst src) (snd src) (fst dest)
        (snd dest))
    valid_moves;
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
     5  | . . . . ♙ . . . |  5\n\
     4  | . . . . ♟ . . . |  4\n\
     3  | . . . . . . . . |  3\n\
     2  | ♙ ♙ ♙ ♙ . ♙ ♙ ♙ |  2\n\
     1  | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |  1\n\
     +    a b c d e f g h    +"
  in
  Printf.printf "Actual board string:\n%s\n" board_str;
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
  Printf.printf "Actual board string:\n%s\n" board_str;
  assert_equal real_str board_str

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
       ]

let () = run_test_tt_main suite
