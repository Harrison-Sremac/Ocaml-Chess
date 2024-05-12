open Types

type difficulty = int

(* Simple evaluation function that counts relative material *)
let evaluate_board board =
  List.fold_left
    (fun acc (_, (piece, color)) ->
      let value =
        match piece with
        | King -> 900
        | Queen -> 90
        | Rook -> 50
        | Bishop -> 30
        | Knight -> 30
        | Pawn -> 10
      in
      match color with
      | White -> acc + value
      | Black -> acc - value)
    0 board

(* Minimax algorithm to determine the best move *)
let rec minimax board color depth alpha beta maximizing_player =
  if depth = 0 then evaluate_board board
  else
    let possible_moves = Board.all_possible_moves board color in
    if maximizing_player then
      List.fold_left
        (fun acc (src, dest) ->
          let new_board = Board.make_move board src dest in
          let score =
            minimax new_board (Board.switch_turn color) (depth - 1) alpha beta
              false
          in
          max acc score)
        min_int possible_moves
    else
      List.fold_left
        (fun acc (src, dest) ->
          let new_board = Board.make_move board src dest in
          let score =
            minimax new_board (Board.switch_turn color) (depth - 1) alpha beta
              true
          in
          min acc score)
        max_int possible_moves

(* Returns the best move for the AI based on the current board state *)
let best_move board color difficulty =
  let _, best_move =
    List.fold_left
      (fun (max_score, best_move) (src, dest) ->
        let new_board = Board.make_move board src dest in
        let score =
          minimax new_board (Board.switch_turn color) (difficulty - 1) min_int
            max_int false
        in
        if score > max_score then (score, (src, dest))
        else (max_score, best_move))
      (min_int, (('-', 0), ('-', 0)))
      (Board.all_possible_moves board color)
  in
  best_move
