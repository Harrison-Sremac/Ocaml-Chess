type color =
  | White
  | Black

type position = int * int

type piece = {
  piece_type : Piece.piece_type;
  color : color;
  possible_moves : position -> color -> position list;
}

let possible_moves (pos : position) : position list =
  let x, y = pos in
  List.init 8 (fun i -> (x + i + 1, y + i + 1))
  @ List.init 8 (fun i -> (x + i + 1, y - i - 1))
  @ List.init 8 (fun i -> (x - i - 1, y + i + 1))
  @ List.init 8 (fun i -> (x - i - 1, y - i - 1))
