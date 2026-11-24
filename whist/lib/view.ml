let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

let print_end_round state (* player *) =
  (* clear_screen (); *)
  let player_id = Model.get_round_winner state in
  Printf.printf "Round over. +1 trick for Team%d\n" (2 - (player_id mod 2));
  (* converts 1,3 -> 1, 2,4 -> 2 *)
  flush stdout

let print_game_state state player =
  clear_screen ();
  let info = Model.get_player_view state player in
  let team_zero, team_one = Model.get_team_scores state in
  let your_score, their_score =
    if info.pi_team = 0 then (team_zero, team_one) else (team_one, team_zero)
  in
  Printf.printf "\n";
  let table = Model.string_of_table info.pi_table (Model.get_user_id player) in
  let hand = Model.string_of_hand info.pi_current_cards in
  Printf.printf "Trick %d of 13 | Trump Suit: %s\n" (info.pi_round_number + 1)
    (Card.string_of_suit info.pi_trump false);
  Printf.printf "Score:  (You) Team 0: %d, (Opponents) Team 1: %d\n" your_score
    their_score;
  Printf.printf "Table: %s\nYour hand: %s\n" table
    (if hand = "" then "(empty)" else hand);
  flush stdout

let print_final state player =
  clear_screen ();
  let info = Model.get_player_view state player in
  let team_zero, team_one = Model.get_team_scores state in
  let your_score, their_score =
    if info.pi_team = 0 then (team_zero, team_one) else (team_one, team_zero)
  in
  let result =
    if your_score > their_score then "Your team won!" else "Your team lost."
  in
  Printf.printf "Game over. %s\nFinal score: your team %d, opponents %d\n"
    result your_score their_score;
  flush stdout
