type color =
  | White
  | Black

type position = int * int

type piece = {
  piece_type : Piece.piece_type;
  color : color;
  possible_moves : position -> color -> position list;
}

let possible_moves (pos : position) (color : color) : position list =
  let x, y = pos in
  match color with
  | White -> if x == 1 then [ (x + 1, y); (x + 2, y) ] else [ (x + 1, y) ]
  | Black -> if x == 6 then [ (x - 1, y); (x - 2, y) ] else [ (x - 1, y) ]
