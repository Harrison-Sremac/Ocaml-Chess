(* Helper function to parse a single chess position from a string like "e2" *)
let parse_position input =
  if String.length input = 2 then
    let file = input.[0] in
    let rank = input.[1] in
    if file >= 'a' && file <= 'h' && rank >= '1' && rank <= '8' then
      Some (file, int_of_char rank - int_of_char '0')
    else None
  else None

(* Reads a move from standard input, expecting two positions separated by a
   space *)
let read_move () =
  print_endline "Enter your move (e.g., e2 e4):";
  try
    let line = read_line () in
    match String.split_on_char ' ' line with
    | [ start; finish ] -> begin
        match (parse_position start, parse_position finish) with
        | Some start_pos, Some finish_pos -> Some (start_pos, finish_pos)
        | _ -> None
      end
    | _ -> None
  with End_of_file -> None
