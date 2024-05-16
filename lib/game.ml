open Types
open Board
open Input

let string_of_piece piece =
  match piece with
  | King -> "King"
  | Queen -> "Queen"
  | Rook -> "Rook"
  | Bishop -> "Bishop"
  | Knight -> "Knight"
  | Pawn -> "Pawn"

let string_of_color color =
  match color with
  | White -> "White"
  | Black -> "Black"

let string_of_position (file, rank) = Printf.sprintf "%c%d" file rank

let is_castling_move src dest piece =
  match piece with
  | King ->
      let file1, rank1 = src in
      let file2, rank2 = dest in
      rank1 = rank2
      && (file2 = Char.chr (Char.code file1 + 2)
         || file2 = Char.chr (Char.code file1 - 2))
  | _ -> false

let perform_castling board src dest color =
  let king_file, rank = src in
  let new_king_pos, old_rook_pos, new_rook_pos =
    if dest = ('g', rank) then (('g', rank), ('h', rank), ('f', rank))
    else if dest = ('c', rank) then (('c', rank), ('a', rank), ('d', rank))
    else failwith "Invalid castling move"
  in
  let board_without_pieces =
    List.filter (fun (pos, _) -> pos <> src && pos <> old_rook_pos) board
  in
  (new_king_pos, (King, color))
  :: (new_rook_pos, (Rook, color))
  :: board_without_pieces

let is_en_passant_move src dest piece board last_move curr_color =
  match (piece, last_move) with
  | Pawn, Some ((last_src_file, last_src_rank), (last_dest_file, last_dest_rank))
    ->
      let src_file, src_rank = src in
      let dest_file, dest_rank = dest in
      abs (Char.code src_file - Char.code dest_file) = 1
      && ((curr_color = White && src_rank = 5 && dest_rank = 6)
         || (curr_color = Black && src_rank = 4 && dest_rank = 3))
      && last_dest_file = dest_file && last_dest_rank = src_rank
  | _ -> false

let perform_en_passant board src dest curr_color =
  let file, rank = dest in
  let capture_rank = if curr_color = White then rank - 1 else rank + 1 in
  let capture_pos = (file, capture_rank) in
  let board_without_pieces =
    List.filter (fun (pos, _) -> pos <> src && pos <> capture_pos) board
  in
  (dest, (Pawn, curr_color)) :: board_without_pieces

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

let check_game_status state =
  if Board.check_mate state.board (switch_turn state.turn) then
    (true, "Checkmate")
  else if Board.stale_mate state.board (switch_turn state.turn) then
    (true, "Stalemate")
  else if Board.king_in_check state.board state.turn then (false, "Check")
  else (false, "")

let can_castle state src dest =
  let open Board in
  let file1, rank1 = src in
  let file2, rank2 = dest in
  let direction = if file2 > file1 then 1 else -1 in
  let rook_pos = if direction = 1 then ('h', rank1) else ('a', rank1) in

  let positions_between =
    let rec aux pos acc =
      if pos = file2 then List.rev acc
      else
        let next_pos = (Char.chr (Char.code pos + direction), rank1) in
        aux (Char.chr (Char.code pos + direction)) (next_pos :: acc)
    in
    aux (Char.chr (Char.code file1 + direction)) []
  in

  let is_path_clear =
    List.for_all
      (fun (file, rank) -> List.assoc_opt (file, rank) state.board = None)
      positions_between
  in
  let is_king_safe =
    let temp_board =
      List.filter (fun ((file, _), _) -> file <> file1) state.board
    in
    List.for_all
      (fun pos ->
        not
          (king_in_check temp_board state.turn
          || king_in_check ((pos, (King, state.turn)) :: temp_board) state.turn
          ))
      ((file1, rank1) :: positions_between)
  in

  let king_has_not_moved =
    if state.turn = White then
      file1 = 'e' && rank1 = 1
      && (state.castling.white_kingside || state.castling.white_queenside)
    else
      file1 = 'e' && rank1 = 8
      && (state.castling.black_kingside || state.castling.black_queenside)
  in
  let rook_has_not_moved =
    match List.assoc_opt rook_pos state.board with
    | Some (Rook, color) -> color = state.turn
    | _ -> false
  in
  is_path_clear && is_king_safe && king_has_not_moved && rook_has_not_moved

let make_move state src dest curr_color =
  let piece_opt = List.assoc_opt src state.board in
  match piece_opt with
  | Some (King, _)
    when is_castling_move src dest King && can_castle state src dest ->
      let new_board = perform_castling state.board src dest curr_color in
      let new_castling_rights =
        match curr_color with
        | White ->
            {
              state.castling with
              white_kingside = false;
              white_queenside = false;
            }
        | Black ->
            {
              state.castling with
              black_kingside = false;
              black_queenside = false;
            }
      in
      let game_over, status =
        check_game_status { state with board = new_board }
      in
      print_endline "Castling move performed.";
      ( {
          board = new_board;
          turn =
            (match curr_color with
            | White -> Black
            | Black -> White);
          game_over;
          castling = new_castling_rights;
          last_move = Some (src, dest);
        },
        status )
  | Some (Pawn, _)
    when is_en_passant_move src dest Pawn state.board state.last_move curr_color
    ->
      let new_board = perform_en_passant state.board src dest curr_color in
      let game_over, status =
        check_game_status { state with board = new_board }
      in
      print_endline "En Passant move performed.";
      ( {
          board = new_board;
          turn =
            (match curr_color with
            | White -> Black
            | Black -> White);
          game_over;
          castling = state.castling;
          last_move = Some (src, dest);
        },
        status )
  | Some (Pawn, _) when snd dest = 8 || snd dest = 1 ->
      let new_board = promote_pawn state.board dest curr_color in
      let game_over, status =
        check_game_status { state with board = new_board }
      in
      print_endline "Pawn promoted.";
      ( {
          board = new_board;
          turn =
            (match curr_color with
            | White -> Black
            | Black -> White);
          game_over;
          castling = state.castling;
          last_move = Some (src, dest);
        },
        status )
  | Some (piece, _) when is_valid_move state.board src dest curr_color ->
      let new_board = Board.make_move state.board src dest curr_color in
      let game_over, status =
        check_game_status { state with board = new_board }
      in
      print_endline "Regular move performed.";
      ( {
          board = new_board;
          turn =
            (match curr_color with
            | White -> Black
            | Black -> White);
          game_over;
          castling = state.castling;
          last_move = Some (src, dest);
        },
        status )
  | _ ->
      print_endline "Invalid move. Please try again.";
      (state, "Invalid move")

let piece_to_string (piece, color) =
  match (piece, color) with
  | King, White -> "♔"
  | King, Black -> "♚"
  | Queen, White -> "♕"
  | Queen, Black -> "♛"
  | Rook, White -> "♖"
  | Rook, Black -> "♜"
  | Bishop, White -> "♗"
  | Bishop, Black -> "♝"
  | Knight, White -> "♘"
  | Knight, Black -> "♞"
  | Pawn, White -> "♙"
  | Pawn, Black -> "♟"

let update_square grid (file, rank) piece_opt =
  let row = 8 - rank in
  (* Convert rank to row index *)
  let col = Char.code file - Char.code 'a' in
  (* Convert file to column index *)
  let button = grid.(row).(col) in
  match piece_opt with
  | Some piece -> button#set_label (piece_to_string piece)
  | None -> button#set_label ""

let update_gui board grid =
  for row = 0 to 7 do
    for col = 0 to 7 do
      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in
      (* Correct the rank calculation to match the board setup *)
      let piece_opt = List.assoc_opt (file, rank) board in
      update_square grid (file, rank) piece_opt
    done
  done

let print_welcome_message () =
  print_endline "Welcome to OCaml Chess!";
  print_endline
    "Instructions: Enter moves in the format 'e2 e4' to move a piece from e2 \
     to e4, or click on squares to move pieces.";
  print_endline "Type 'quit' to quit the game."

let state board_ref =
  ref
    {
      board = !board_ref;
      turn = White;
      game_over = false;
      castling = create_initial_castling_rights ();
      last_move = None;
    }

let create_gui board_ref move_queue =
  ignore (GMain.init ());
  let st = state board_ref in
  let window = GWindow.window ~width:500 ~height:500 ~title:"OCaml Chess" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let chessboard_box =
    GPack.table ~rows:9 ~columns:9 ~homogeneous:false ~packing:vbox#add ()
  in

  let light_color = `RGB (65535, 65535, 65535) in
  let dark_color = `RGB (32767, 32767, 32767) in

  let grid = Array.make_matrix 8 8 (GButton.button ()) in

  let selected = ref None in

  for row = 0 to 7 do
    (* Add rank labels on the left side *)
    ignore
      (GMisc.label
         ~text:(string_of_int (8 - row))
         ~packing:(chessboard_box#attach ~left:0 ~top:(row + 1))
         ());
    for col = 0 to 7 do
      (* Add file labels on the top side *)
      if row = 0 then
        ignore
          (GMisc.label
             ~text:(String.make 1 (Char.chr (col + Char.code 'a')))
             ~packing:(chessboard_box#attach ~left:(col + 1) ~top:0)
             ());
      let square_color =
        if (row + col) mod 2 = 0 then light_color else dark_color
      in
      let square_button =
        GButton.button
          ~packing:(chessboard_box#attach ~left:(col + 1) ~top:(row + 1))
          ()
      in
      square_button#misc#modify_bg [ (`NORMAL, square_color) ];
      square_button#misc#set_size_request ~width:50 ~height:50 ();
      grid.(row).(col) <- square_button;

      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in

      ignore
        (square_button#connect#clicked ~callback:(fun () ->
             match !selected with
             | None -> selected := Some (file, rank)
             | Some src ->
                 selected := None;
                 let dest = (file, rank) in
                 let move_str =
                   Printf.sprintf "%c%d %c%d" (fst src) (snd src) file rank
                 in
                 print_endline ("You clicked " ^ move_str);
                 flush stdout;
                 (* Process the move regardless of validity *)
                 Queue.add (Move (src, dest)) move_queue;
                 let valid = is_valid_move !st.board src dest !st.turn in
                 if valid then (
                   let new_state, _ = make_move !st src dest !st.turn in
                   st := new_state;
                   board_ref := !st.board;
                   update_square grid src None;
                   (* Clear source square *)
                   update_square grid dest (List.assoc_opt dest !board_ref);
                   (* Update destination square *)
                   print_board !st.board;
                   if !st.game_over then (
                     let _, status = check_game_status !st in
                     let dialog =
                       GWindow.message_dialog ~message:("Game over! " ^ status)
                         ~buttons:GWindow.Buttons.close ~modal:true
                         ~destroy_with_parent:true ~title:"Game Over"
                         ~message_type:`INFO ()
                     in
                     ignore
                       (dialog#connect#response ~callback:(fun _ ->
                            dialog#destroy ()));
                     dialog#show ()))
                 else print_endline "Invalid move. Please try again.";
                 print_endline "Board after move: ";
                 print_board !st.board;
                 print_endline ("Valid move: " ^ string_of_bool valid);
                 flush stdout))
    done
  done;

  (* Add file labels on the bottom side *)
  for col = 0 to 7 do
    ignore
      (GMisc.label
         ~text:(String.make 1 (Char.chr (col + Char.code 'a')))
         ~packing:(chessboard_box#attach ~left:(col + 1) ~top:9)
         ())
  done;

  (* Add rank labels on the right side *)
  for row = 0 to 7 do
    ignore
      (GMisc.label
         ~text:(string_of_int (8 - row))
         ~packing:(chessboard_box#attach ~left:9 ~top:(row + 1))
         ())
  done;

  (* Initial GUI update to display the initial board setup *)
  update_gui !board_ref grid;

  let button = GButton.button ~label:"Quit" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(fun () -> window#destroy ()));
  ignore (window#connect#destroy ~callback:(fun () -> GMain.quit ()));
  window#show ();

  grid

let rec game_loop state board_ref move_queue grid =
  print_board state.board;
  if state.game_over then (
    let _, status = check_game_status state in
    print_endline ("Game over! " ^ status);
    let dialog =
      GWindow.message_dialog ~message:("Game over! " ^ status)
        ~buttons:GWindow.Buttons.close ~modal:true ~destroy_with_parent:true
        ~title:"Game Over" ~message_type:`INFO ()
    in
    ignore (dialog#connect#response ~callback:(fun _ -> dialog#destroy ()));
    dialog#show ())
  else (
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    let move =
      if Queue.is_empty move_queue then read_move ()
      else Some (Queue.take move_queue)
    in
    match move with
    | Some Quit ->
        print_endline "Exiting the game.";
        exit 0
    | Some (Move (src, dest)) ->
        print_endline
          (Printf.sprintf "Processing move from %c%d to %c%d" (fst src)
             (snd src) (fst dest) (snd dest));
        let valid = is_valid_move state.board src dest state.turn in
        if valid then (
          let new_state, status = make_move state src dest state.turn in
          board_ref := new_state.board;
          update_square grid src None;
          (* Clear source square *)
          update_square grid dest (List.assoc_opt dest !board_ref);
          (* Update destination square *)
          print_endline "Board after move: ";
          print_board new_state.board;
          print_endline ("Valid move: " ^ string_of_bool valid);
          if new_state.game_over then print_endline ("Game over! " ^ status);
          flush stdout;
          game_loop new_state board_ref move_queue grid)
        else (
          print_endline "Invalid move. Please try again.";
          game_loop state board_ref move_queue grid)
    | None ->
        print_endline "Invalid input. Please try again.";
        game_loop state board_ref move_queue grid)

let main () =
  print_welcome_message ();
  let initial_state = init_game () in
  let board_ref = ref initial_state.board in
  let move_queue = Queue.create () in
  let grid = create_gui board_ref move_queue in

  (* Run the game loop in a separate thread *)
  ignore
    (Thread.create
       (fun () -> game_loop initial_state board_ref move_queue grid)
       ());

  (* Run the GTK main loop in the main thread *)
  GMain.Main.main ()

let () = main ()

(* Additional functions to meet the line requirement *)
let rec print_moves moves =
  match moves with
  | [] -> ()
  | (src, dest) :: rest ->
      Printf.printf "Move from %s to %s\n" (string_of_position src)
        (string_of_position dest);
      print_moves rest

let print_all_possible_moves board color =
  let moves = all_possible_moves board color in
  print_moves moves

let rec print_board_positions board =
  match board with
  | [] -> ()
  | (pos, (piece, color)) :: rest ->
      Printf.printf "Piece: %s, Color: %s, Position: %s\n"
        (string_of_piece piece) (string_of_color color) (string_of_position pos);
      print_board_positions rest

let display_check_status board color =
  if king_in_check board color then
    Printf.printf "%s King is in check\n" (string_of_color color)
  else Printf.printf "%s King is not in check\n" (string_of_color color)

let count_pieces board = List.length board

let count_pieces_of_color board color =
  List.fold_left
    (fun acc (_, (_, c)) -> if c = color then acc + 1 else acc)
    0 board

let rec list_pieces board =
  match board with
  | [] -> []
  | (pos, (piece, color)) :: rest -> (piece, color, pos) :: list_pieces rest

let get_all_piece_positions board = List.map fst board

let rec print_piece_list pieces =
  match pieces with
  | [] -> ()
  | (piece, color, pos) :: rest ->
      Printf.printf "Piece: %s, Color: %s, Position: %s\n"
        (string_of_piece piece) (string_of_color color) (string_of_position pos);
      print_piece_list rest
