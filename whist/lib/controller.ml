(**[choose_valid_card model curr_player] is a helper function that asks the
   [curr_player] to choose a valid card. Returns the chosen card.*)
let rec choose_valid_card model curr_player =
  let chosen_card = Cli.get_card () in
  if Model.valid_move model curr_player chosen_card then chosen_card
  else (
    print_endline "Card is invalid this round, please choose again.";
    choose_valid_card model curr_player)

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
      if Model.get_user_id curr_player <> 0 then
        Bot.next_move (Model.get_player_view model curr_player)
      else choose_valid_card model curr_player
    in
    let model = Model.make_move model curr_player chosen_card in
    View.print_game_state model;
    round_loop (player_num + 1) model

(**[run seed] starts the main controller loop using the given [seed] to
   initialize a model.*)
let run seed =
  let rng = Random.State.make [| seed |] in
  let rec play_rounds remaining model =
    if remaining > 0 then (
      let completed_round = round_loop 0 model in
      View.print_game_state completed_round;
      if remaining > 1 then
        play_rounds (remaining - 1) (Model.new_round completed_round))
  in
  play_rounds 13 (Model.init rng)
