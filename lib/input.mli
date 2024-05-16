(* @author Ajay Tadinada (at663), Harrison Sremac (hcs59), Mericel Tao (mst223),
   Sanya Kohli (sk2682) *)

(** Represents a command issued by the user: either a move or quitting the game. *)
type user_command =
  | Move of (char * int) * (char * int)
  | Quit

val read_move : unit -> user_command option
(** [read_move ()] reads a move or a quit command from standard input. Returns
    [Some user_command] if the input is valid, or [None] if the input is invalid
    or end of input is reached. *)
