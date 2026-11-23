

let rec get_card error_msg =
  (match error_msg with
  | Some em -> print_endline em
  | None -> ());

  print_endline "Enter a card (hit enter to see format info): ";
  let open Card in
  let card = read_line () in
  match String.length card with
  | 2 ->
      (try Card.card_of_string card with
      | Exceptions.MalformedCard em -> get_card (Some em))
  | 0 ->
      print_endline
        "The first character is the rank (2-10 = number values, J = Jack, Q = \
         Queen, K = King, A = Ace) and the second is the suit (H = \
         Hearts,S,D,C); ex. AD = Ace of Diamonds, 3C = 3 of Clubs)";
      get_card None
  | _ ->
      print_endline "Invalid format, must be 2 characters only";
      get_card None
