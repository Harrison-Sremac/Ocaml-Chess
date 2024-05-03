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
  [
    (x + 2, y + 1);
    (x + 2, y - 1);
    (x - 2, y + 1);
    (x - 2, y - 1);
    (x + 1, y + 2);
    (x + 1, y - 2);
    (x - 1, y + 2);
    (x - 1, y - 2);
  ]
