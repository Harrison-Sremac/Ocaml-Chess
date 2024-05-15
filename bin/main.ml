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
  | King, White -> "♔"
  | King, Black -> "♚"
  | Queen, White -> "♕"
  | Queen, Black -> "♛"
  | Rook, White -> "♖"
  | Rook, Black -> "♜"
  | Bishop, White -> "♗"
  | Bishop, Black -> "♝"
  | Knight, White -> "♘"
  | Knight, Black -> "♞"
  | Pawn, White -> "♙"
  | Pawn, Black -> "♟"

let update_square grid (file, rank) piece_opt =
  let row = 8 - rank in
  (* Convert rank to row index *)
  let col = Char.code file - Char.code 'a' in
  (* Convert file to column index *)
  let button = grid.(row).(col) in
  match piece_opt with
  | Some piece -> button#set_label (piece_to_string piece)
  | None -> button#set_label ""

let update_gui board grid =
  for row = 0 to 7 do
    for col = 0 to 7 do
      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in
      (* Correct the rank calculation to match the board setup *)
      let piece_opt = List.assoc_opt (file, rank) board in
      update_square grid (file, rank) piece_opt
    done
  done

let state board_ref =
  ref
    {
      board = !board_ref;
      turn = White;
      game_over = false;
      castling = create_initial_castling_rights ();
      last_move = None;
    }

let create_gui board_ref move_queue =
  ignore (GMain.init ());
  let st = state board_ref in
  let window = GWindow.window ~width:500 ~height:500 ~title:"OCaml Chess" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let chessboard_box =
    GPack.table ~rows:9 ~columns:9 ~homogeneous:false ~packing:vbox#add ()
  in

  let light_color = `RGB (65535, 65535, 65535) in
  let dark_color = `RGB (32767, 32767, 32767) in

  let grid = Array.make_matrix 8 8 (GButton.button ()) in

  let selected = ref None in

  for row = 0 to 7 do
    (* Add rank labels on the left side *)
    ignore
      (GMisc.label
         ~text:(string_of_int (8 - row))
         ~packing:(chessboard_box#attach ~left:0 ~top:(row + 1))
         ());
    for col = 0 to 7 do
      (* Add file labels on the top side *)
      if row = 0 then
        ignore
          (GMisc.label
             ~text:(String.make 1 (Char.chr (col + Char.code 'a')))
             ~packing:(chessboard_box#attach ~left:(col + 1) ~top:0)
             ());
      let square_color =
        if (row + col) mod 2 = 0 then light_color else dark_color
      in
      let square_button =
        GButton.button
          ~packing:(chessboard_box#attach ~left:(col + 1) ~top:(row + 1))
          ()
      in
      square_button#misc#modify_bg [ (`NORMAL, square_color) ];
      square_button#misc#set_size_request ~width:50 ~height:50 ();
      grid.(row).(col) <- square_button;

      let file = Char.chr (col + Char.code 'a') in
      let rank = 8 - row in

      ignore
        (square_button#connect#clicked ~callback:(fun () ->
             match !selected with
             | None -> selected := Some (file, rank)
             | Some src ->
                 selected := None;
                 let dest = (file, rank) in
                 let move_str =
                   Printf.sprintf "%c%d %c%d" (fst src) (snd src) file rank
                 in
                 print_endline ("You clicked " ^ move_str);
                 flush stdout;
                 (* Process the move regardless of validity *)
                 Queue.add (Move (src, dest)) move_queue;
                 if is_valid_move !st.board src dest !st.turn then (
                   let new_state = make_move !st src dest !st.turn in
                   st := new_state;
                   board_ref := !st.board;
                   update_square grid src None;
                   (* Clear source square *)
                   update_square grid dest (List.assoc_opt dest !board_ref);
                   (* Update destination square *)
                   print_board !st.board)
                 else print_endline "Invalid move. Please try again.";
                 print_board !st.board))
    done
  done;

  (* Add file labels on the bottom side *)
  for col = 0 to 7 do
    ignore
      (GMisc.label
         ~text:(String.make 1 (Char.chr (col + Char.code 'a')))
         ~packing:(chessboard_box#attach ~left:(col + 1) ~top:9)
         ())
  done;

  (* Add rank labels on the right side *)
  for row = 0 to 7 do
    ignore
      (GMisc.label
         ~text:(string_of_int (8 - row))
         ~packing:(chessboard_box#attach ~left:9 ~top:(row + 1))
         ())
  done;

  (* Initial GUI update to display the initial board setup *)
  update_gui !board_ref grid;

  let button = GButton.button ~label:"Quit" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(fun () -> window#destroy ()));
  ignore (window#connect#destroy ~callback:(fun () -> GMain.quit ()));
  window#show ();

  grid

let rec game_loop state board_ref move_queue grid =
  print_board state.board;
  if state.game_over then
    let _, status = check_game_status state in
    print_endline ("Game over! " ^ status)
  else (
    print_endline
      (match state.turn with
      | White -> "White's move:"
      | Black -> "Black's move:");
    let move =
      if Queue.is_empty move_queue then read_move ()
      else Some (Queue.take move_queue)
    in
    match move with
    | Some Quit ->
        print_endline "Exiting the game.";
        exit 0
    | Some (Move (src, dest)) ->
        print_endline
          (Printf.sprintf "Processing move from %c%d to %c%d" (fst src)
             (snd src) (fst dest) (snd dest));
        let new_state =
          if is_valid_move state.board src dest state.turn then
            make_move state src dest state.turn
          else (
            print_endline "Invalid move. Please try again.";
            state)
        in
        board_ref := new_state.board;
        update_square grid src None;
        (* Clear source square *)
        update_square grid dest (List.assoc_opt dest !board_ref);
        (* Update destination square *)
        print_board new_state.board;
        game_loop new_state board_ref move_queue grid
    | None ->
        print_endline "Invalid input. Please try again.";
        game_loop state board_ref move_queue grid)

let main () =
  print_welcome_message ();
  let initial_state = init_game () in
  let board_ref = ref initial_state.board in
  let move_queue = Queue.create () in
  let grid = create_gui board_ref move_queue in

  (* Run the game loop in a separate thread *)
  ignore
    (Thread.create
       (fun () -> game_loop initial_state board_ref move_queue grid)
       ());

  (* Run the GTK main loop in the main thread *)
  GMain.Main.main ()

let () = main ()
