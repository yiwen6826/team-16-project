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
  | 2 | 3 ->
      let suit_index = String.length card - 1 in
      begin match rank_of_string_opt (String.sub card 0 suit_index) with
      | Some rank ->
          begin match
            suit_of_string_opt
              (String.uppercase_ascii (String.sub card suit_index 1))
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
        "Enter a rank (2-10, J, Q, K, A) followed by a suit (H, S, D, C). \
         For example, AD is the Ace of Diamonds and 10C is the 10 of Clubs.";
      get_card ()
  | _ ->
      print_endline "Invalid format. Enter a rank and suit, such as AD or 10C.";
      get_card ()
