(* val print_game_state : Model.state -> Model.player -> unit *)
let clear_screen () =
  print_string "\027[3J\027[2J\027[H";
  flush stdout


let print_game_state state player =
  clear_screen ();
  let pi = Model.get_player_view state player in
  print_endline
    ("current suit: "
    ^ Card.string_of_suit_option pi.pi_current_suit
    ^ ", trump suit: "
    ^ Card.string_of_suit_option (Some pi.pi_trump)
    ^ ", current player: "
    ^ string_of_int pi.pi_current_player);

  let table_str =
    Array.to_list pi.pi_table |> List.map Card.string_of_card_option
  in
  print_endline ("table: " ^ String.concat ", " table_str);

  let hand_str =
    List.map (fun x -> Card.string_of_card_option (Some x)) pi.pi_current_cards
  in
  print_endline ("your hand: " ^ String.concat ", " hand_str)

let print_round_end state player = 
  print_game_state state player;
  print_endline ("round over. winner: " ^ string_of_int (Model.get_round_winner state))