open Batteries

module CharSet = Set.Make (Char)
(**[CharSet] is a [Set] of module [Char] *)

(**[no_repeat_letters word] returns true if [word] has no repeat letters and
   false otherwise. Sources:
   https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatString.html,
   lines 11-12, date accessed 3/25/24; https://v2.ocaml.org/api/Set.S.html, line
   12, date accessed 3/25/24*)
let no_repeat_letters word =
  let letters_set = word |> BatString.to_list |> CharSet.of_list in
  CharSet.cardinal letters_set = BatString.length word

(* creates lists of possible words, source:
   https://github.com/ConorSheehan1/spelling-bee/blob/main/data/AllWords.txt,
   date accessed 3/25/24*)
let word_list = BatList.of_enum (BatFile.lines_of "data/AllWords.txt")

(** [initalize] will begin the game thinking. [word_list] reads the list of
    valid and possible Spelling Bee words. [reandom_seven_letter_word lst] finds
    a random five letter word with no duplicate letters from [lst]. Requires:
    [lst] is not empty. *)
let initialize () =
  Random.self_init ();
  let random_seven_letter_word lst =
    let seven_list =
      List.filter (fun x -> String.length x = 7 && no_repeat_letters x) lst
    in
    let index = Random.int (List.length seven_list) in
    List.nth seven_list index
  in
  (* Creates a list of chars made up of the letters of the random word. Source:
     https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatList.html,
     line 34, date accessed 3/25/24. Source:
     https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatString.html,
     line 34, date accessed 3/25/24 *)
  let letters =
    BatList.shuffle (BatString.to_list (random_seven_letter_word word_list))
  in

  (* Chooses a random letter to be the central letter *)
  let central_letter =
    let center_index = Random.int (List.length letters) in
    List.nth letters center_index
  in
  (letters, central_letter)

(** [is_valid_word word central_letter letters word_list] checks to see if
    [word] is a valid, point-earning word given the game's [central letter],
    letter list [letters], and possible words in [word_list]. Source:
    https://v2.ocaml.org/api/String.html, lines 53-54, date accessed 3/25/24.
    Source:
    https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatSet.html,
    lines 53, 56, date accessed 3/25/24. *)
let is_valid_word (word : string) central_letter letters word_list =
  let bat_word_list = BatSet.of_list word_list in
  let has_central_letter = String.contains word central_letter in
  let uses_valid_letters = String.for_all (fun c -> List.mem c letters) word in
  let is_known_word = BatSet.mem word bat_word_list in
  has_central_letter && uses_valid_letters && is_known_word
