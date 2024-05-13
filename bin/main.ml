open Chess.Board
open Chess.Types
open Chess.Input
open Chess.Game

let print_welcome_message () =
  print_endline "Welcome to OCaml Chess!";
  print_endline
    "Instructions: Enter moves in the format 'e2 e4' to move a piece from e2 \
     to e4.";
  print_endline "Type 'quit' to quit the game."

let create_gui board =
  ignore (GtkMain.Main.init ());
  let window = GWindow.window ~width:400 ~height:400 ~title:"OCaml Chess" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let chessboard_box = GPack.vbox ~packing:vbox#add () in

  let light_color = `RGB (65535, 65535, 65535) in
  let dark_color = `RGB (0, 0, 0) in

  let piece_to_string (piece, color) =
    match (piece, color) with
    | King, White -> "♚"
    | King, Black -> "♔"
    | Queen, White -> "♛"
    | Queen, Black -> "♕"
    | Rook, White -> "♜"
    | Rook, Black -> "♖"
    | Bishop, White -> "♝"
    | Bishop, Black -> "♗"
    | Knight, White -> "♞"
    | Knight, Black -> "♘"
    | Pawn, White -> "♟"
    | Pawn, Black -> "♙"
  in

  for row = 0 to 7 do
    let hbox = GPack.hbox ~packing:chessboard_box#add () in
    for col = 0 to 7 do
      let square_color =
        if (row + col) mod 2 = 0 then light_color else dark_color
      in
      let square_button = GButton.button ~packing:hbox#add () in
      square_button#misc#modify_bg [ (`NORMAL, square_color) ];
      square_button#misc#set_size_request ~width:50 ~height:50 ();

      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in
      let position = (file, rank) in

      (match List.assoc_opt position board with
      | Some piece -> square_button#set_label (piece_to_string piece)
      | None -> ());

      square_button#misc#show ()
    done
  done;

  let button = GButton.button ~label:"Quit" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(fun () -> GMain.quit ()));
  ignore (window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.Main.main ()

let rec game_loop state =
  print_board state.board;
  if state.game_over then
    let _, status = check_game_status state in
    print_endline ("Game over! " ^ status)
  else (
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    match read_move () with
    | Some Quit ->
        print_endline "Exiting the game.";
        exit 0
    | Some (Move (src, dest)) ->
        let new_state = make_move state src dest state.turn in
        game_loop new_state
    | None ->
        print_endline "Invalid input. Please try again.";
        game_loop state)

let main () =
  let initial_state = init_game () in
  create_gui initial_state.board;
  print_welcome_message ();
  game_loop initial_state

let () = main ()
