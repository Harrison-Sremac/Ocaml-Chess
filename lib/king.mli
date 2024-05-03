type color =
  | White
  | Black

type piece_type = King

type piece = {
  piece_type : piece_type;
  color : color;
}

val string_of_piece : piece -> string
