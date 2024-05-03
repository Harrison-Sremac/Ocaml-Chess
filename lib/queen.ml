type color =
  | White
  | Black

type piece_type = Queen

type piece = {
  piece_type : piece_type;
  color : color;
}

let string_of_piece p =
  let color_char =
    match p.color with
    | White -> 'W'
    | Black -> 'B'
  in
  Printf.sprintf "Q%c" color_char
