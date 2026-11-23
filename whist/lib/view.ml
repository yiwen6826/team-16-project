let suit_name = function
  | Card.Hearts -> "Hearts"
  | Card.Spades -> "Spades"
  | Card.Diamonds -> "Diamonds"
  | Card.Clubs -> "Clubs"

let card_code { Card.rank; suit } =
  let rank_text =
    match rank with
    | Card.Number n -> string_of_int n
    | Card.Jack -> "J"
    | Card.Queen -> "Q"
    | Card.King -> "K"
    | Card.Ace -> "A"
  in
  let suit_code = (suit_name suit).[0] in
  Printf.sprintf "%s%c" rank_text suit_code

let print_game_state state player =
  let info = Model.get_player_view state player in
  let team_zero, team_one = Model.get_scores state in
  let your_score, their_score =
    if info.team = 0 then (team_zero, team_one) else (team_one, team_zero)
  in
  Printf.printf "\n";
  if info.round_number = 13 then
    let result =
      if your_score > their_score then "Your team won!" else "Your team lost."
    in
    Printf.printf "Game over. %s\nFinal score: your team %d, opponents %d\n"
      result your_score their_score
  else
    let viewer_id = Model.get_user_id player in
    let table =
      Model.get_table_cards info.table
      |> List.mapi (fun id card ->
          let player_label =
            if id = viewer_id then "You" else Printf.sprintf "Player %d" (id + 1)
          in
          let played = Option.fold ~none:"--" ~some:card_code card in
          Printf.sprintf "%s [%s]" player_label played)
      |> String.concat "  "
    in
    let hand = String.concat "  " (List.map card_code info.current_cards) in
    Printf.printf "Trick %d of 13 | Trump: %s\n" (info.round_number + 1)
      (suit_name info.trump);
    Printf.printf "Score: your team %d, opponents %d\n" your_score their_score;
    Printf.printf "Table: %s\nYour hand: %s\n" table
      (if hand = "" then "(empty)" else hand)
