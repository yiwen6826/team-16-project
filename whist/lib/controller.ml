(**[choose_valid_card model curr_player] is a helper function that asks the
   [curr_player] to choose a valid card. Returns the chosen card.*)
let rec choose_valid_card model curr_player =
  let chosen_card = Cli.get_card () in
  let rejection =
    match Model.valid_move model curr_player chosen_card with
    | true -> None
    | false -> Some "That card cannot be played this turn."
    | exception Exceptions.CardNotHeld -> Some "That card is not in your hand."
    | exception Exceptions.IllegalCard -> Some "You have to follow suit."
  in
  match rejection with
  | None -> chosen_card
  | Some reason ->
      print_endline reason;
      choose_valid_card model curr_player

let human_player_id = 0

let show_game_state model =
  View.print_game_state model (Model.get_player_by_id model human_player_id)

(**[round_loop player_num model] is a helper function that loops through a
   round/trick. Start with [player_num] = 0 and loops 4 times, making one move
   per player. Returns the updated model. [player_num] is not the same as
   [player_id], is simply keeping track of how many players have played without
   knowing who they are.*)
let rec round_loop player_num model =
  if player_num = 4 then model
  else
    let curr_player = Model.get_current_player model in
    let chosen_card =
      if Model.get_user_id curr_player <> human_player_id then
        Bot.next_move (Model.get_player_view model curr_player)
      else (
        show_game_state model;
        choose_valid_card model curr_player)
    in
    let model = Model.make_move model curr_player chosen_card in
    round_loop (player_num + 1) model

(**[run seed] starts the main controller loop using the given [seed] to
   initialize a model.*)
let run seed =
  let rng = Random.State.make [| seed |] in
  let rec play_rounds remaining model =
    let completed_round = round_loop 0 model in
    show_game_state completed_round;
    let next_round = Model.new_round completed_round in
    if remaining = 1 then show_game_state next_round
    else play_rounds (remaining - 1) next_round
  in
  play_rounds 13 (Model.init rng)
