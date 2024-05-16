(* @author Ajay Tadinada (at663), Harrison Sremac (hcs59), Mericel Tao (mst223),
   Sanya Kohli (sk2682) *)

type user_command =
  | Move of (char * int) * (char * int)
  | Quit

(** [parse_position input] makes sense of the positions input by the user *)
let parse_position input =
  if String.length input = 2 then
    let file = input.[0] in
    let rank = input.[1] in
    if file >= 'a' && file <= 'h' && rank >= '1' && rank <= '8' then
      Some (file, Char.code rank - Char.code '0')
    else None
  else None

let read_move () =
  print_endline "Enter your move (e.g., e2 e4) or type 'quit' to end the game:";
  try
    let line = read_line () in
    if line = "quit" then Some Quit
    else
      match String.split_on_char ' ' line with
      | [ start; finish ] -> (
          match (parse_position start, parse_position finish) with
          | Some start_pos, Some finish_pos ->
              Some (Move (start_pos, finish_pos))
          | _ ->
              print_endline "Invalid move format. Please enter again.";
              None)
      | _ ->
          print_endline "Invalid input format. Please enter again.";
          None
  with End_of_file ->
    print_endline "End of input. Exiting.";
    None
