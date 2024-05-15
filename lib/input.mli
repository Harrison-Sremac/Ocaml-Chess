type user_command =
  | Move of (char * int) * (char * int)
  | Quit

val read_move : unit -> user_command option
