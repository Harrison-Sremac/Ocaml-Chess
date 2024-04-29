(* piece.ml *)
type color =
  | White
  | Black

type piece_type =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

type piece = {
  piece_type : piece_type;
  color : color;
}

let string_of_piece p =
  let color_char =
    match p.color with
    | White -> 'W'
    | Black -> 'B'
  in
  match p.piece_type with
  | King -> Printf.sprintf "K%c" color_char
  | Queen -> Printf.sprintf "Q%c" color_char
  | Rook -> Printf.sprintf "R%c" color_char
  | Bishop -> Printf.sprintf "B%c" color_char
  | Knight -> Printf.sprintf "N%c" color_char
  | Pawn -> Printf.sprintf "P%c" color_char
