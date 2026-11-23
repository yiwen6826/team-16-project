let rank_of_string_opt rank =
  match int_of_string_opt rank with
  | Some r when r >= 2 && r <= 10 -> Some (Card.Number r)
  | None ->
      begin match rank with
      | "A" -> Some Card.Ace
      | "K" -> Some King
      | "Q" -> Some Queen
      | "J" -> Some Jack
      | _ -> None
      end
  | _ -> None

let suit_of_string_opt suit =
  match suit with
  | "H" -> Some Card.Hearts
  | "S" -> Some Spades
  | "D" -> Some Diamonds
  | "C" -> Some Clubs
  | _ -> None

let rec get_card () =
  print_endline "Enter a card (hit enter to see format info): ";
  let open Card in
  let card = read_line () in
  match String.length card with
  | 2 ->
      begin match rank_of_string_opt (String.sub card 0 1) with
      | Some rank ->
          begin match
            suit_of_string_opt (String.uppercase_ascii (String.sub card 1 1))
          with
          | Some suit -> { Card.rank; suit }
          | None ->
              print_endline
                "Invalid suit, must be H = Hearts, S = Spades, D = Diamonds, C \
                 = Clubs";
              get_card ()
          end
      | None ->
          print_endline
            "Invalid rank, must be between 2 and 10, inclusive, or J,Q,K,A";
          get_card ()
      end
  | 0 ->
      print_endline
        "The first character is the rank (2-10 = number values, J = Jack, Q = \
         Queen, K = King, A = Ace) and the second is the suit (H = \
         Hearts,S,D,C); ex. AD = Ace of Diamonds, 3C = 3 of Clubs)";
      get_card ()
  | _ ->
      print_endline "Invalid format, must be 2 characters only";
      get_card ()
