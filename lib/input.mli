type user_command =
  | Move of (char * int) * (char * int)
  | Quit

(* Reads a move or a quit command from standard input *)
val read_move : unit -> user_command option
