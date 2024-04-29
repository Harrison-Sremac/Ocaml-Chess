(* board.ml *)
open Piece

(* We define the board as a matrix of piece option to represent the pieces on
   the board *)
type board = piece option array array

(* Initialize an empty board *)
let init_board () : board = Array.make_matrix 8 8 None

(* Set up the initial positions of pieces on the board *)
let setup_board (board : board) : unit =
  (* Helper function to place a full row of pawns *)
  let place_pawns color row =
    Array.fill board.(row) 0 8 (Some { color; piece_type = Pawn })
  in
  (* Helper function to place the major pieces *)
  let place_major_pieces color row =
    board.(row) <-
      [|
        Some { color; piece_type = Rook };
        Some { color; piece_type = Knight };
        Some { color; piece_type = Bishop };
        Some { color; piece_type = Queen };
        Some { color; piece_type = King };
        Some { color; piece_type = Bishop };
        Some { color; piece_type = Knight };
        Some { color; piece_type = Rook };
      |]
  in
  place_pawns Black 1;
  place_major_pieces Black 0;
  place_pawns White 6;
  place_major_pieces White 7

(* Print the board to the console *)
let print_board (board : board) : unit =
  let string_of_piece = function
    | None -> ". "
    | Some p -> string_of_piece p
  in
  Array.iter
    (fun row ->
      Array.iter (fun piece -> print_string (string_of_piece piece ^ " ")) row;
      print_endline "")
    board

(* Example usage *)
let () =
  let board = init_board () in
  setup_board board;
  print_board board
