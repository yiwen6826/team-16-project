let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

let print_game_state state player =
  clear_screen ();
  let info = Model.get_player_view state player in
  let team_zero, team_one = Model.get_team_scores state in
  let your_score, their_score =
    if info.pi_team = 0 then (team_zero, team_one) else (team_one, team_zero)
  in
  Printf.printf "\n";
  if info.pi_round_number = 13 then (* Final State printout *)
    let result =
      if your_score > their_score then "Your team won!" else "Your team lost."
    in
    Printf.printf "Game over. %s\nFinal score: your team %d, opponents %d\n"
      result your_score their_score
  else (* During game printout *)
    let table =
      Model.string_of_table
        (info.pi_table)
        (Model.get_user_id player)
    in
    let hand = Model.string_of_hand info.pi_current_cards in
    Printf.printf "Trick %d of 13 | Trump Suit: %s\n" (info.pi_round_number + 1)
      (Card.string_of_suit info.pi_trump false);
    Printf.printf "Score: your team %d, opponents %d\n" your_score their_score;
    Printf.printf "Table: %s\nYour hand: %s\n" table
      (if hand = "" then "(empty)" else hand)
  ;
  flush stdout
