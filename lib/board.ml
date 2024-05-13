open Types

type position = char * int
type board = (position * (piece * color)) list

let create_initial_castling_rights () =
  {
    white_kingside = true;
    white_queenside = true;
    black_kingside = true;
    black_queenside = true;
  }

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
        | King, White -> "♚"
        | King, Black -> "♔"
        | Queen, White -> "♛"
        | Queen, Black -> "♕"
        | Rook, White -> "♜"
        | Rook, Black -> "♖"
        | Bishop, White -> "♝"
        | Bishop, Black -> "♗"
        | Knight, White -> "♞"
        | Knight, Black -> "♘"
        | Pawn, White -> "♟"
        | Pawn, Black -> "♙"
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

let is_valid_move board src dest curr_color =
  match List.assoc_opt src board with
  | Some (piece, color) ->
      if color = curr_color then
        let moves = Pieces.possible_moves piece color src board in
        List.exists (fun (_, end_pos) -> end_pos = dest) moves
      else false
  | None -> false

let make_move board src dest curr_color =
  if is_valid_move board src dest curr_color then
    match List.assoc_opt src board with
    | Some piece ->
        let board_without_src =
          List.filter (fun (pos, _) -> pos <> src) board
        in
        let board_without_dest =
          List.filter (fun (pos, _) -> pos <> dest) board_without_src
        in
        (dest, piece) :: board_without_dest
    | None -> board
  else board

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
  (pos, (piece, color)) :: List.filter (fun (p, _) -> p <> pos) board

let check_mate _board = false
let stale_mate _board = false

let all_possible_moves board color =
  List.fold_left
    (fun acc (pos, (piece, piece_color)) ->
      if piece_color = color then
        let piece_moves = Pieces.possible_moves piece color pos board in
        List.append piece_moves acc
      else acc)
    [] board

let switch_turn color =
  match color with
  | White -> Black
  | Black -> White

let print_board board = print_endline (board_to_string board)
