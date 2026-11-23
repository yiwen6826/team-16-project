(**[choose_valid_card model curr_player] is a helper function that asks the
   [curr_player] to choose a valid card. Returns the chosen card.*)
let rec choose_valid_card model curr_player =
  let chosen_card = Cli.get_card None in
  try 
  if Model.valid_move model curr_player chosen_card then chosen_card
  else (
    print_endline "Card is invalid this round, please choose again.";
    choose_valid_card model curr_player) with
  | Exceptions.OutOfOrder -> print_endline "it is not your turn"; choose_valid_card model curr_player
  | Exceptions.CardNotHeld -> print_endline "you do not own that card"; choose_valid_card model curr_player
  | Exceptions.IllegalCard -> print_endline "you cannot play that card since you have one of the suit in play"; choose_valid_card model curr_player

(**[round_loop player_num model] is a helper function that loops through a
   round/trick. Start with [player_num] = 0 and loops 4 times, making one move
   per player. Returns the updated model. [player_num] is not the same as
   [player_id], is simply keeping track of how many players have played without
   knowing who they are.*)
(* let rec round_loop player_num model =
  if player_num = 4 then model
  else
    let curr_player = Model.get_current_player model in
    let chosen_card =
      if Model.get_user_id curr_player <> 0 then
        Bot.next_move (Model.get_player_view model curr_player)
      else choose_valid_card model curr_player
    in
    let model = Model.make_move model curr_player chosen_card in
    View.print_game_state model curr_player;
    round_loop (player_num + 1) model *)

(**[run seed] starts the main controller loop using the given [seed] to
   initialize a model.*)
let run seed =
  let rng = Random.State.make [| seed |] in
  let state = ref (Model.init rng) in

  for x = 1 to 13 do
    for y = 0 to 3 do
      let current_player = Model.get_current_player !state in
      View.print_game_state !state (Model.get_player_by_id !state 0); (* print terminal view for human player*)
      let card =
        if Model.get_user_id current_player = 0 then
          begin
          choose_valid_card !state current_player
          end
        else 
          let miu = Bot.next_move (Model.get_player_view !state current_player) in
          if Model.valid_move !state current_player miu then miu else raise Exceptions.MalformedInternalArg
      in
      state := Model.make_move !state current_player card
    done;
    View.print_round_end !state (Model.get_player_by_id !state 0);
    Unix.sleep 2;

    state := Model.new_round !state

  done
