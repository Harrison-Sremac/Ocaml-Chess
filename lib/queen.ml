type color =
  | White
  | Black

type position = int * int

type piece = {
  piece_type : Piece.piece_type;
  color : color;
  possible_moves : position -> color -> position list;
}

(* Possible moves for a Queen *)
let possible_moves (pos : position) : position list =
  let x, y = pos in
  let line_moves =
    List.init 8 (fun i -> (x + i + 1, y))
    @ List.init 8 (fun i -> (x - i - 1, y))
    @ List.init 8 (fun i -> (x, y + i + 1))
    @ List.init 8 (fun i -> (x, y - i - 1))
  in
  let diag_moves =
    List.init 8 (fun i -> (x + i + 1, y + i + 1))
    @ List.init 8 (fun i -> (x + i + 1, y - i - 1))
    @ List.init 8 (fun i -> (x - i - 1, y + i + 1))
    @ List.init 8 (fun i -> (x - i - 1, y - i - 1))
  in
  line_moves @ diag_moves
