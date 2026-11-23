let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

(** takes a [Card.suit] input and returns its string representation, as one
    letter if [short] else the whole string *)
let string_of_suit suit short =
  let str =
    match suit with
    | Card.Hearts -> "Hearts" (* Hearts *)
    | Card.Spades -> "Spades" (* Spades *)
    | Card.Diamonds -> "Diamonds" (* Diamonds *)
    | Card.Clubs -> "Clubs" (* Clubs *)
  in
  if short then String.sub str 0 1 else str

(** takes a [Card.rank] input and returns its string representation *)
let string_of_rank = function
  | Card.Number n -> string_of_int n
  | Card.Jack -> "J"
  | Card.Queen -> "Q"
  | Card.King -> "K"
  | Card.Ace -> "A"

(** takes a [Card.card] input and returns its string representation *)
let string_of_card { Card.rank; suit } =
  Printf.sprintf "%s%s" (string_of_rank rank) (string_of_suit suit true)

(** takes a [Model.table] input (exposed as [Card.card option list]) and returns
    its string representation *)
let string_of_table table viewer_id =
  table
  |> List.mapi (fun id card ->
      let player_label =
        if id = viewer_id then "You" else Printf.sprintf "Player %d" (id + 1)
      in
      let played =
        match card with
        | None -> "XX"
        | Some c -> string_of_card c
      in
      Printf.sprintf "%s [%s]" player_label played)
  |> String.concat "  "

(** takes a [hand] input (exposed as [Card.card list]) and returns its string
    representation *)
let string_of_hand hand =
  List.fold_left (fun acc card -> acc ^ string_of_card card) "" hand

let print_game_state state player =
  let info = Model.get_player_view state player in
  let team_zero, team_one = Model.get_scores state in
  let your_score, their_score =
    if info.team = 0 then (team_zero, team_one) else (team_one, team_zero)
  in
  Printf.printf "\n";
  if info.round_number = 13 then (* Final State printout *)
    let result =
      if your_score > their_score then "Your team won!" else "Your team lost."
    in
    Printf.printf "Game over. %s\nFinal score: your team %d, opponents %d\n"
      result your_score their_score
  else (* During game printout *)
    let table =
      string_of_table
        (Model.get_table_cards info.table)
        (Model.get_user_id player)
    in
    let hand = string_of_hand info.current_cards in
    Printf.printf "Trick %d of 13 | Trump Suit: %s\n" (info.round_number + 1)
      (string_of_suit info.trump false);
    Printf.printf "Score: your team %d, opponents %d\n" your_score their_score;
    Printf.printf "Table: %s\nYour hand: %s\n" table
      (if hand = "" then "(empty)" else hand)
