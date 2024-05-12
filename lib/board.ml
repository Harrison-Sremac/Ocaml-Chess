type piece =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

type color =
  | White
  | Black

type position = char * int (* e.g., ('e', 2) *)
type board = (position * (piece * color)) list

(* Helper function to create a row of pawns for a given color *)
let create_pawn_row color rank =
  List.map
    (fun file -> ((file, rank), (Pawn, color)))
    [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

(* Helper function to create the main piece row for a given color *)
let create_piece_row color rank =
  [
    (('a', rank), (Rook, color));
    (('b', rank), (Knight, color));
    (('c', rank), (Bishop, color));
    (('d', rank), (Queen, color));
    (('e', rank), (King, color));
    (('f', rank), (Bishop, color));
    (('g', rank), (Knight, color));
    (('h', rank), (Rook, color));
  ]

(* Initializes the chessboard with pieces at starting positions *)
let initialize_board () : board =
  create_piece_row White 1 @ create_pawn_row White 2 @ create_pawn_row Black 7
  @ create_piece_row Black 8

(* Converts the board to a string representation *)
(* Converts the board to a string representation with file and rank labels *)
let board_to_string board =
  let empty_line = " +-a-b-c-d-e-f-g-h-+" in
  let rows = Array.init 8 (fun _ -> Array.make 8 '.') in
  List.iter
    (fun ((file, rank), (piece, color)) ->
      let symbol =
        match (piece, color) with
        | King, White -> 'K'
        | King, Black -> 'k'
        | Queen, White -> 'Q'
        | Queen, Black -> 'q'
        | Rook, White -> 'R'
        | Rook, Black -> 'r'
        | Bishop, White -> 'B'
        | Bishop, Black -> 'b'
        | Knight, White -> 'N'
        | Knight, Black -> 'n'
        | Pawn, White -> 'P'
        | Pawn, Black -> 'p'
      in
      rows.(8 - rank).(Char.code file - Char.code 'a') <- symbol)
    board;
  let numbered_rows =
    Array.mapi
      (fun i row ->
        string_of_int (8 - i)
        ^ " |"
        ^ (Array.to_list row |> List.map Char.escaped |> String.concat " ")
        ^ "| "
        ^ string_of_int (8 - i))
      rows
  in
  String.concat "\n"
    ([ empty_line ] @ Array.to_list numbered_rows @ [ empty_line ])

(* Checks if moving from src to dest is a valid move in the given board state *)
let is_valid_move board src dest =
  let src_file, src_rank = src in
  let dest_file, dest_rank = dest in
  List.exists (fun (pos, _) -> pos = src) board
  && src_file >= 'a' && src_file <= 'h' && src_rank >= 1 && src_rank <= 8
  && dest_file >= 'a' && dest_file <= 'h' && dest_rank >= 1 && dest_rank <= 8

(* Moves a piece from source to destination, returning the new board state *)
let make_move board src dest =
  if is_valid_move board src dest then
    match List.assoc_opt src board with
    | Some piece ->
        let board_without_src =
          List.filter (fun (pos, _) -> pos <> src) board
        in
        (dest, piece) :: board_without_src
    | None -> board (* No piece at src; return original board *)
  else board (* Invalid move; return original board *)

(* Generates all possible moves for a given color *)
let all_possible_moves board color =
  let add_moves_for_piece acc (pos, (piece, piece_color)) =
    if piece_color = color then
      let moves =
        match piece with
        | Pawn ->
            [ (fst pos, snd pos + if piece_color = White then 1 else -1) ]
            (* Simplified move logic for pawn, forward only *)
        | Rook ->
            List.init 7 (fun i -> (fst pos, snd pos + i + 1))
            (* Simplified logic for Rook, moving vertically down *)
        | _ -> [] (* Placeholder for other pieces *)
      in
      List.fold_left (fun acc move -> (pos, move) :: acc) acc moves
    else acc
  in
  List.fold_left add_moves_for_piece [] board

(* Implementation of switch_turn to toggle between White and Black *)
let switch_turn color =
  match color with
  | White -> Black
  | Black -> White

(* Prints the board to the terminal *)
let print_board board = print_endline (board_to_string board)

(* Placeholder for checking checkmate condition *)
let check_mate _board = false

(* Placeholder for checking stalemate condition *)
let stale_mate _board = false
