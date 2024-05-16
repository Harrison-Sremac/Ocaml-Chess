open Types

type position = char * int
type board = (position * (piece * color)) list

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
let piece_at_position board pos = List.assoc_opt pos board
let board_as_list board = board

let create_initial_castling_rights () =
  {
    white_kingside = true;
    white_queenside = true;
    black_kingside = true;
    black_queenside = true;
  }
  [@coverage off]

let create_pawn_row color rank =
  List.map
    (fun file -> ((file, rank), (Pawn, color)))
    [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

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

let initialize_board () =
  create_piece_row White 1 @ create_pawn_row White 2 @ create_pawn_row Black 7
  @ create_piece_row Black 8

let board_to_string board =
  let empty_line = "+    a b c d e f g h    +" in
  let rows = Array.init 8 (fun _ -> Array.make 8 ".") in
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

let king_in_check board color =
  let king_pos =
    List.find (fun (_, (piece, c)) -> piece = King && c = color) board |> fst
  in
  List.exists
    (fun (pos, (piece, c)) ->
      c <> color
      && List.exists
           (fun (_, end_pos) -> end_pos = king_pos)
           (Pieces.possible_moves piece c pos board))
    board

let make_move board src dest curr_color =
  match List.assoc_opt src board with
  | Some piece ->
      let board_without_src = List.filter (fun (pos, _) -> pos <> src) board in
      let board_without_dest =
        List.filter (fun (pos, _) -> pos <> dest) board_without_src
      in
      (dest, piece) :: board_without_dest
  | None -> board

let is_valid_move board src dest curr_color =
  match List.assoc_opt src board with
  | Some (piece, color) ->
      if color = curr_color then
        let possible_moves = Pieces.possible_moves piece color src board in
        List.exists (fun (_, end_pos) -> end_pos = dest) possible_moves
        && not (king_in_check (make_move board src dest curr_color) curr_color)
      else false
  | None -> false

let move_piece board src dest = make_move board src dest White

let promote_pawn board pos color =
  let piece_choice = "Queen" in
  let piece =
    match piece_choice with
    | "Queen" -> Queen
    | "Rook" -> Rook
    | "Bishop" -> Bishop
    | "Knight" -> Knight
    | _ -> Queen
  in
  let board_without_pawn = List.filter (fun (p, _) -> p <> pos) board in
  (pos, (piece, color)) :: board_without_pawn

let all_possible_moves board color =
  List.fold_left
    (fun acc (pos, (piece, piece_color)) ->
      if piece_color = color then
        let piece_moves = Pieces.possible_moves piece color pos board in
        List.append piece_moves acc
      else acc)
    [] board

let check_mate board color =
  if king_in_check board color then
    let moves = all_possible_moves board color in
    List.for_all
      (fun (src, dest) ->
        let new_board = make_move board src dest color in
        king_in_check new_board color)
      moves
  else false

let stale_mate board color =
  (not (king_in_check board color))
  &&
  let moves = all_possible_moves board color in
  List.for_all
    (fun (src, dest) ->
      let new_board = make_move board src dest color in
      king_in_check new_board color)
    moves

let switch_turn color =
  match color with
  | White -> Black
  | Black -> White

let valid_moves_for_piece board pos =
  match List.assoc_opt pos board with
  | Some (piece, color) -> Pieces.possible_moves piece color pos board
  | None -> []

let is_checkmate board color = check_mate board color
let is_stalemate board color = stale_mate board color
let initialize_castling_rights () = create_initial_castling_rights ()
let print_board board = (print_endline (board_to_string board) [@coverage off])
