open Game

(** [game_st] holds the game state including the list of letters and the central
    letter. *)
let game_st = initialize ()

(** [game_loop ()] prompts the user for words, checks their validity, and
    continues until the user quits. *)
let rec game_loop () =
  print_endline "Enter a word (or 'quit' to exit): ";
  let user_word = read_line () in
  if user_word = "quit" then print_endline "Goodbye!"
  else
    let letters, central_letter = game_st in
    if is_valid_word user_word central_letter letters then
      print_endline "Valid word!"
    else print_endline "Invalid word.";
    game_loop ()

(** [main ()] initializes and starts the game loop. *)
let () =
  print_endline "Welcome to the Spelling Bee Game!";
  let letters, central_letter = game_st in
  let letters_string =
    List.map (fun c -> String.make 1 c) letters |> String.concat ""
  in
  print_endline ("Use these letters to create words: " ^ letters_string);
  print_endline
    ("The central letter that must be in every word is: "
    ^ String.make 1 central_letter);
  game_loop ()
