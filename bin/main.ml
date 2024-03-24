let letters = [ 'e'; 'a'; 'r'; 't'; 'h'; 's'; 'o' ]
let central_letter = 'e'

let valid_words =
  [
    "earth";
    "heart";
    "rate";
    "hate";
    "heat";
    "eat";
    "tea";
    "sea";
    "ear";
    "tear";
    "shear";
  ]

let is_valid_word word =
  let has_central_letter = String.contains word central_letter in
  let uses_valid_letters = String.for_all (fun c -> List.mem c letters) word in
  let is_known_word = List.mem word valid_words in
  has_central_letter && uses_valid_letters && is_known_word

let rec game_loop () =
  print_endline "Enter a word (or 'quit' to exit): ";
  let user_word = read_line () in
  if user_word = "quit" then print_endline "Goodbye!"
  else (
    if is_valid_word user_word then print_endline "Valid word!"
    else print_endline "Invalid word.";
    game_loop ())

let () =
  let letters_string = List.map (String.make 1) letters |> String.concat "" in
  print_endline
    ("Welcome to the Spelling Bee Game!\nUse these letters to create words: "
   ^ letters_string);
  print_endline
    ("The central letter that must be in every word is: "
    ^ String.make 1 central_letter);
  game_loop ()
