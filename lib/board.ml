open Piece

type board = piece option array array

let init_board () : board = Array.make_matrix 8 8 None

let setup_board (board : board) : unit =
  (* Helper function to place a full row of pawns *)
  let place_pawns color row =
    for i = 0 to 7 do
      board.(row).(i) <- Some { color; piece_type = Pawn }
    done
  in

  (* Helper function to place the major pieces *)
  let place_major_pieces color row =
    board.(row).(0) <- Some { color; piece_type = Rook };
    board.(row).(1) <- Some { color; piece_type = Knight };
    board.(row).(2) <- Some { color; piece_type = Bishop };
    board.(row).(3) <- Some { color; piece_type = Queen };
    board.(row).(4) <- Some { color; piece_type = King };
    board.(row).(5) <- Some { color; piece_type = Bishop };
    board.(row).(6) <- Some { color; piece_type = Knight };
    board.(row).(7) <- Some { color; piece_type = Rook }
  in
  place_pawns Black 1;
  place_major_pieces Black 0;
  place_pawns White 6;
  place_major_pieces White 7

(*let how_to_move (p : piece) = match p.piece_type with | Rook ->
  Rook.possible_moves | Knight -> Knight.possible_moves | Bishop ->
  Bishop.possible_moves | Queen -> Queen.possible_moves | King ->
  King.possible_moves | _ -> failwith "Invalid piece type"*)

(*let move_piece (board : board) start_pos end_pos : unit = match board.(fst
  start_pos).(snd start_pos) with | Some p -> if List.mem end_pos (how_to_move p
  start_pos) then begin board.(fst end_pos).(snd end_pos) <- Some p; board.(fst
  start_pos).(snd start_pos) <- None end else Printf.printf "Invalid move\n" |
  None -> Printf.printf "No piece at starting position\n" *)

let print_board (board : board) : unit =
  let string_of_piece = function
    | None -> ".  "
    | Some p -> string_of_piece p ^ " "
  in
  print_string "    ";
  Array.iter
    (fun c -> Printf.printf "%c  " c)
    [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' |];
  print_endline "";
  Array.iteri
    (fun i row ->
      Printf.printf "%d  " (i + 1);
      Array.iter (fun piece -> print_string (string_of_piece piece)) row;
      print_endline "")
    board

let () =
  let board = init_board () in
  setup_board board;
  print_board board
