open Chess.Board
open Chess.Types
open Chess.Input
open Chess.Game

let print_welcome_message () =
  print_endline "Welcome to OCaml Chess!";
  print_endline
    "Instructions: Enter moves in the format 'e2 e4' to move a piece from e2 \
     to e4, or click on squares to move pieces.";
  print_endline "Type 'quit' to quit the game."

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

let update_gui board grid =
  for row = 0 to 7 do
    for col = 0 to 7 do
      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in
      let button = grid.(row).(col) in
      match List.assoc_opt (file, rank) board with
      | Some piece -> button#set_label (piece_to_string piece)
      | None -> button#set_label ""
    done
  done

let create_gui board_ref =
  ignore (GtkMain.Main.init ());
  let window = GWindow.window ~width:400 ~height:400 ~title:"OCaml Chess" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let chessboard_box = GPack.vbox ~packing:vbox#add () in

  let light_color = `RGB (65535, 65535, 65535) in
  let dark_color = `RGB (0, 0, 0) in

  let grid = Array.make_matrix 8 8 (GButton.button ()) in

  for row = 0 to 7 do
    let hbox = GPack.hbox ~packing:chessboard_box#add () in
    for col = 0 to 7 do
      let square_color =
        if (row + col) mod 2 = 0 then light_color else dark_color
      in
      let square_button = GButton.button ~packing:hbox#add () in
      square_button#misc#modify_bg [ (`NORMAL, square_color) ];
      square_button#misc#set_size_request ~width:50 ~height:50 ();
      grid.(row).(col) <- square_button;

      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in

      ignore
        (square_button#connect#clicked ~callback:(fun () ->
             Printf.printf "You clicked %c%d\n" file rank;
             flush stdout));

      square_button#misc#show ()
    done
  done;

  let button = GButton.button ~label:"Quit" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(fun () -> window#destroy ()));
  ignore (window#connect#destroy ~callback:(fun () -> GMain.quit ()));
  window#show ();

  let update_interval = 100 in
  (* Update every 100 milliseconds *)
  ignore
    (GMain.Timeout.add ~ms:update_interval ~callback:(fun () ->
         update_gui !board_ref grid;
         true (* Continue calling this function *)));
  GMain.Main.main ()

let rec game_loop state board_ref move_callback =
  print_board state.board;
  if state.game_over then
    let _, status = check_game_status state in
    print_endline ("Game over! " ^ status)
  else (
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    match move_callback () with
    | Some Quit ->
        print_endline "Exiting the game.";
        exit 0
    | Some (Move (src, dest)) ->
        let new_state = make_move state src dest state.turn in
        board_ref := new_state.board;
        game_loop new_state board_ref move_callback
    | None ->
        print_endline "Invalid input. Please try again.";
        game_loop state board_ref move_callback)

let main () =
  print_welcome_message ();
  let initial_state = init_game () in
  let board_ref = ref initial_state.board in
  let move_queue = Queue.create () in

  let move_callback () =
    if Queue.is_empty move_queue then
      match read_move () with
      | Some move -> Some move
      | None -> None
    else Some (Queue.take move_queue)
  in

  ignore (Thread.create (fun () -> create_gui board_ref) ());
  game_loop initial_state board_ref move_callback

let () = main ()
