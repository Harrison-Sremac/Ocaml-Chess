(* @author Ajay Tadinada (at663), Harrison Sremac (hcs59), Mericel Tao (mst223),
   Sanya Kohli (sk2682) *)

type piece =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn

type color =
  | White
  | Black

type position = char * int (* e.g., ('e', 2) *)
type board = (position * (piece * color)) list

type castling_rights = {
  white_kingside : bool;
  white_queenside : bool;
  black_kingside : bool;
  black_queenside : bool;
}

type move = position * position
