type color =
  | White
  | Black

type position = int * int

type piece = {
  piece_type : Piece.piece_type;
  color : color;
  possible_moves : position -> color -> position list;
}

(* Possible moves for a King *)
let possible_moves (pos : position) : position list =
  let x, y = pos in
  [
    (x + 1, y);
    (x - 1, y);
    (x, y + 1);
    (x, y - 1);
    (x + 1, y + 1);
    (x + 1, y - 1);
    (x - 1, y + 1);
    (x - 1, y - 1);
  ]
