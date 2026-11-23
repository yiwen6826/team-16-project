open Whist

(** [get_seed ()] prompts the user for an integer and returns it. If the user
    enters a non-integer, the function prints a message and prompts again. *)
let rec get_seed () =
  print_endline "Enter a seed to shuffle the cards: ";
  try int_of_string (String.trim (read_line ()))
  with Failure _ ->
    print_endline "Please enter a valid integer.";
    get_seed ()

(** [main ()] initializes and runs the game. *)
let main () =
  print_endline
    "Welcome to Whist! The year is 1820 and you and your partner must play \
     your cards right to stay afloat...";
  let seed = get_seed () in

  Controller.run seed

let () = main ()
