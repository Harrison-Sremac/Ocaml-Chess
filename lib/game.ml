open Types
open Board

let is_castling_move src dest piece =
  match piece with
  | King ->
      let file1, rank1 = src in
      let file2, rank2 = dest in
      rank1 = rank2
      && (file2 = char_of_int (Char.code file1 + 2)
         || file2 = char_of_int (Char.code file1 - 2))
  | _ -> false

let perform_castling board src dest color =
  let king_file, rank = src in
  let new_king_pos, old_rook_pos, new_rook_pos =
    if dest = ('g', rank) then
      ( (char_of_int (Char.code king_file + 2), rank),
        ('h', rank),
        (char_of_int (Char.code king_file + 1), rank) )
    else if dest = ('c', rank) then
      ( (char_of_int (Char.code king_file - 2), rank),
        ('a', rank),
        (char_of_int (Char.code king_file - 1), rank) )
    else failwith "Invalid castling move"
  in
  let board_without_pieces =
    List.filter (fun (pos, _) -> pos <> src && pos <> old_rook_pos) board
  in
  (new_king_pos, (King, color))
  :: (new_rook_pos, (Rook, color))
  :: board_without_pieces

let is_en_passant_move src dest piece board last_move =
  match piece with
  | Pawn ->
      let src_file, src_rank = src in
      let dest_file, dest_rank = dest in
      let last_src, last_dest =
        match last_move with
        | Some mv -> mv
        | None -> (('x', 0), ('x', 0))
      in
      let last_src_file, last_src_rank = last_src in
      let last_dest_file, last_dest_rank = last_dest in
      last_src_rank = 7 && last_dest_rank = 5 && dest_rank = 6
      && last_src_file = src_file && last_dest_file = dest_file
  | _ -> false

let perform_en_passant board src dest =
  let file, rank = dest in
  let capture_pos = (file, rank - 1) in
  let board_without_pieces =
    List.filter (fun (pos, _) -> pos <> src && pos <> capture_pos) board
  in
  (dest, (Pawn, White)) :: board_without_pieces

type game_state = {
  board : Board.board;
  turn : Types.color;
  game_over : bool;
  castling : castling_rights;
  last_move : move option;
}

let init_game () =
  {
    board = Board.initialize_board ();
    turn = Types.White;
    game_over = false;
    castling = create_initial_castling_rights ();
    last_move = None;
  }

let switch_turn turn =
  match turn with
  | Types.White -> Types.Black
  | Types.Black -> Types.White

let check_game_status state =
  if Board.check_mate state.board then (true, "Checkmate")
  else if Board.stale_mate state.board then (true, "Stalemate")
  else (false, "")

let make_move state src dest curr_color =
  let piece_opt = List.assoc_opt src state.board in
  match piece_opt with
  | Some (King, _) when is_castling_move src dest King ->
      let new_board = perform_castling state.board src dest curr_color in
      let new_castling_rights = create_initial_castling_rights () in
      let game_over = check_mate new_board || stale_mate new_board in
      {
        board = new_board;
        turn = switch_turn curr_color;
        game_over;
        castling = new_castling_rights;
        last_move = Some (src, dest);
      }
  | Some (Pawn, _)
    when is_en_passant_move src dest Pawn state.board state.last_move ->
      let new_board = perform_en_passant state.board src dest in
      let game_over = check_mate new_board || stale_mate new_board in
      {
        board = new_board;
        turn = switch_turn curr_color;
        game_over;
        castling = state.castling;
        last_move = Some (src, dest);
      }
  | Some (Pawn, _) when snd dest = 8 || snd dest = 1 ->
      let new_board = promote_pawn state.board dest curr_color in
      let game_over = check_mate new_board || stale_mate new_board in
      {
        board = new_board;
        turn = switch_turn curr_color;
        game_over;
        castling = state.castling;
        last_move = Some (src, dest);
      }
  | _ ->
      if is_valid_move state.board src dest curr_color then
        let new_board = Board.make_move state.board src dest curr_color in
        let game_over = check_mate new_board || stale_mate new_board in
        {
          board = new_board;
          turn = switch_turn curr_color;
          game_over;
          castling = state.castling;
          last_move = Some (src, dest);
        }
      else (
        print_endline "Invalid move. Please try again.";
        state)
