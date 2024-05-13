open Types

type position = char * int
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
(* Initializes the chessboard with pieces at starting positions *)
let initialize_board () =
  create_piece_row White 1 @ create_pawn_row White 2 @ create_pawn_row Black 7
  @ create_piece_row Black 8

(* Converts the board to a string representation with file and rank labels *)
let board_to_string board =
  let empty_line = "+    a b c d e f g h    +" in
  let rows = Array.init 8 (fun _ -> Array.make 8 ".") in
  (* Initialize with "." instead of '.' *)
  List.iter
    (fun ((file, rank), (piece, color)) ->
      let symbol =
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
      in
      rows.(8 - rank).(Char.code file - Char.code 'a') <- symbol)
    board;
  let numbered_rows =
    Array.mapi
      (fun i row ->
        string_of_int (8 - i)
        ^ "  | "
        ^ (Array.to_list row |> String.concat " ")
        ^ " |  "
        ^ string_of_int (8 - i))
      rows
  in
  String.concat "\n"
    ([ empty_line ] @ Array.to_list numbered_rows @ [ empty_line ])

(* Checks if a move is valid by ensuring it is one of the possible moves for the
   piece at src *)
let is_valid_move board src dest curr_color =
  match List.assoc_opt src board with
  | Some (piece, color) ->
      if color = curr_color then
        let moves = Pieces.possible_moves piece color src board in
        List.exists (fun (_, end_pos) -> end_pos = dest) moves
      else false
  | None -> false

(* Moves a piece from source to destination, returning the new board state *)
let make_move board src dest curr_color =
  if is_valid_move board src dest curr_color then
    match List.assoc_opt src board with
    | Some piece ->
        let board_without_src =
          List.filter (fun (pos, _) -> pos <> src) board
        in
        (dest, piece) :: board_without_src
    | None -> board (* No piece at src; return original board *)
  else board (* Invalid move; return original board *)

(* Placeholder for checking checkmate and stalemate conditions *)
let check_mate _board = false
let stale_mate _board = false

(* Generates all possible moves for a given color, used for check/checkmate
   evaluation *)
let all_possible_moves board color =
  List.fold_left
    (fun acc (pos, (piece, piece_color)) ->
      if piece_color = color then
        let piece_moves = Pieces.possible_moves piece color pos board in
        List.append piece_moves acc
      else acc)
    [] board

(* Switches the current player's turn *)
let switch_turn color =
  match color with
  | White -> Black
  | Black -> White

(* Prints the board to the terminal *)
let print_board board = print_endline (board_to_string board)
