(* Initializes game. Also saves important information in tuple [game_st]*)
let game_st = Cs3110_final_project.Game.initialize ()

(** returns the first entry of the tuple [st] *)
let fst st =
  match st with
  | x, _ -> x

(** returns the second entry of the tuple [st] *)
let snd st =
  match st with
  | _, x -> x

(** [game_loop ()] asks for a guess and checks for validity. It ends the game
    when the user types "quit" *)
let rec game_loop () =
  print_endline "Enter a word (or 'quit' to exit): ";
  let user_word = read_line () in
  if user_word = "quit" then print_endline "Goodbye!"
  else (
    if
      Cs3110_final_project.Game.is_valid_word user_word (snd game_st)
        (fst game_st) Cs3110_final_project.Game.word_list
    then print_endline "Valid word!"
    else print_endline "Invalid word.";
    game_loop ())

(** Starts the game by printing all necessary information *)
let () =
  Random.self_init ();
  let letters_string =
    List.map (String.make 1) (fst game_st) |> String.concat ""
  in
  print_endline
    ("Welcome to the Spelling Bee Game!\nUse these letters to create words: "
   ^ letters_string);
  print_endline
    ("The central letter that must be in every word is: "
    ^ String.make 1 (snd game_st));
  game_loop ()
