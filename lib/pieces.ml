open Types

type position = char * int (* e.g., ('e', 2) *)
type board = (position * (piece * color)) list
type move = position * position

(* Helper function to create moves within board limits *)
let within_board (file, rank) =
  file >= 'a' && file <= 'h' && rank >= 1 && rank <= 8

(* Generate possible moves for a King *)
let king_moves color (file, rank) board =
  let directions =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  in
  let make_move (df, dr) =
    let new_file = char_of_int (Char.code file + df) in
    let new_rank = rank + dr in
    let new_pos = (new_file, new_rank) in
    if within_board new_pos then
      match List.assoc_opt new_pos board with
      | None -> Some new_pos
      | Some (_, piece_color) ->
          if piece_color <> color then Some new_pos else None
    else None
  in
  let regular_moves =
    directions |> List.filter_map make_move
    |> List.map (fun dest -> ((file, rank), dest))
  in

  (* Check for castling moves *)
  let castling_moves =
    let can_castle_kingside = true in
    let can_castle_queenside = true in
    let kingside_move =
      if can_castle_kingside then [ ((file, rank), ('g', rank)) ] else []
    in
    let queenside_move =
      if can_castle_queenside then [ ((file, rank), ('c', rank)) ] else []
    in
    kingside_move @ queenside_move
  in

  regular_moves @ castling_moves

let linear_moves color (file, rank) board directions =
  let rec add_moves pos direction acc =
    let df, dr = direction in
    let new_file = char_of_int (Char.code (fst pos) + df) in
    let new_rank = snd pos + dr in
    let new_pos = (new_file, new_rank) in
    if within_board new_pos then
      match List.assoc_opt new_pos board with
      | Some (_, piece_color) when piece_color = color -> acc
      | Some _ -> new_pos :: acc
      | None -> add_moves new_pos direction (new_pos :: acc)
    else acc
  in
  List.fold_left (fun acc dir -> add_moves (file, rank) dir acc) [] directions

let queen_moves color position board =
  let directions =
    [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (1, 1); (-1, 1); (1, -1) ]
  in
  linear_moves color position board directions
  |> List.map (fun end_pos -> (position, end_pos))

let rook_moves color position board =
  let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] in
  linear_moves color position board directions
  |> List.map (fun end_pos -> (position, end_pos))

let bishop_moves color position board =
  let directions = [ (-1, -1); (1, 1); (-1, 1); (1, -1) ] in
  linear_moves color position board directions
  |> List.map (fun end_pos -> (position, end_pos))

let knight_moves color (file, rank) board =
  let moves =
    [ (-2, -1); (-1, -2); (1, -2); (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1) ]
  in
  let make_move (df, dr) =
    let new_file = char_of_int (Char.code file + df) in
    let new_rank = rank + dr in
    let new_pos = (new_file, new_rank) in
    if within_board new_pos then
      match List.assoc_opt new_pos board with
      | None -> Some new_pos
      | Some (_, piece_color) ->
          if piece_color <> color then Some new_pos else None
    else None
  in
  List.filter_map make_move moves |> List.map (fun dest -> ((file, rank), dest))

let pawn_moves color (file, rank) board =
  let forward = if color = White then 1 else -1 in
  let start_rank = if color = White then 2 else 7 in
  let single_step_pos = (file, rank + forward) in
  let double_step_pos = (file, rank + (2 * forward)) in
  let moves =
    match List.assoc_opt single_step_pos board with
    | None ->
        let single_move = [ ((file, rank), single_step_pos) ] in
        if rank = start_rank && List.assoc_opt double_step_pos board = None then
          single_move @ [ ((file, rank), double_step_pos) ]
        else single_move
    | Some _ -> []
  in
  let captures =
    List.filter_map
      (fun (df, _) ->
        let capture_pos = (char_of_int (Char.code file + df), rank + forward) in
        if within_board capture_pos then
          match List.assoc_opt capture_pos board with
          | Some (_, piece_color) when piece_color <> color ->
              Some ((file, rank), capture_pos)
          | _ -> None
        else None)
      [ (-1, 0); (1, 0) ]
  in
  moves @ captures

let possible_moves piece color position board =
  match piece with
  | King -> king_moves color position board
  | Queen -> queen_moves color position board
  | Rook -> rook_moves color position board
  | Bishop -> bishop_moves color position board
  | Knight -> knight_moves color position board
  | Pawn -> pawn_moves color position board
