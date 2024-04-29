(* Main.ml *)

let () =
  (* Initialize and setup the game *)
  let board = Chess.init_game () in
  (* Possibly load resources, set up game parameters, etc. *)
  Chess.start_game board
