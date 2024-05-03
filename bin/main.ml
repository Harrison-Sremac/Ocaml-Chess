(* Main.ml *)
open Chess

let () =
  (* Initialize and setup the game *)
  let board = init_game () in
  (* Possibly load resources, set up game parameters, etc. *)
  Chess.start_game board
