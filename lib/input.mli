(* Parses user input into a position type *)
val parse_position : string -> (char * int) option

(* Reads a move from standard input and returns start and end positions if
   valid *)
val read_move : unit -> ((char * int) * (char * int)) option
