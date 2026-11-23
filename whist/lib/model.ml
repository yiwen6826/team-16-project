(** Rep. Invariant - in whist, the number of cards in a hand in always >=0 and
    <= 13 AF : any [None] represents a lack of a card, whereas a [Some card] is
    an existing card of some valid suit and rank *)
type hand = Card.card list
(** card list *)

type player = {
  hand : hand;
  user_id : int;
      (** should this signify real user vs. bot? or do we need another *)
  team : int;
}

type table = Card.card option array
(** Type table (field in the state record) uses type card array, but its RI is
    the number of cards is >=0 and <= 4 AF: same as the AF for type hand, but
    indices 0,1,2,3 correlate to the card played by players 1,2,3,4,
    respectively, such that a [None] indicates the player has not yet taken
    their turn, and a [Some card] means the player has places this card on the
    table, and {i it no longer exists in their hand} *)

type player_information = {
  pi_current_suit : Card.suit option;
  pi_trump : Card.suit;
  pi_current_cards : hand;
  pi_table : table;
  pi_team : int;
  pi_round_number : int;
  pi_current_player : int;
}
(* some information that cana be used by bots for planning a move, or displayed
   to users in the terminal *)

type round_state = {
  number : int;
  current_suit : Card.suit option;
  first_player : int;
  current_player : int;
  table : table;
  trump_suit : Card.suit;
}
(* information pertaning to the current round *)

type state = {
  players : player array;
  round_state : round_state;
  scores : int * int;
}
(* let to_option arr = Array.map (fun card -> if card.rank = Number 0 then None
   else Some card) arr *)
(* information for the game overall*)

let sub_list arr start len = List.init len (fun i -> arr.(start + i))
(* helper to get sub array as a list *)

let init seed =
  let deck = Array.copy Card.full_deck in
  Array.shuffle ~rand:Random.int deck;

  {
    players =
      [|
        { hand = sub_list deck 0 13; user_id = 0; team = 0 };
        { hand = sub_list deck 13 13; user_id = 1; team = 1 };
        { hand = sub_list deck 26 13; user_id = 2; team = 0 };
        { hand = sub_list deck 39 13; user_id = 3; team = 1 };
      |];
    round_state =
      {
        number = 0;
        first_player = 0;
        current_suit = None;
        current_player = 0;
        table = Array.init 4 (fun _ -> None);
        trump_suit = Hearts;
      };
    scores = (0, 0);
  }
(* initialize game state at the beginning of the game *)

let end_round state =
  let rs = state.round_state in
  let current_suit =
    match rs.current_suit with 
    | None -> raise Exceptions.MalformedInternalArg
    | Some c -> c
  in
  let winner = Card.max_card rs.table rs.trump_suit current_suit in
  let new_score =
    match winner with
    | 0 | 2 -> (fst state.scores + 1, snd state.scores)
    | 1 | 3 -> (fst state.scores, snd state.scores + 1)
    | _ -> raise (Invalid_argument "invalid winner")
  in
  {
    state with 
    scores = new_score;
  }
  

let new_round state =
  let rs = state.round_state in
  let current_suit =
    match rs.current_suit with
    | None -> invalid_arg "new round triggered out of order"
    | Some c -> c
  in
  let winner = Card.max_card rs.table rs.trump_suit current_suit
  in
  {
    state with
    round_state =
      {
        rs with
        first_player = winner;
        number = rs.number + 1;
        current_suit = None;
        current_player = winner;
        table = [| None; None; None; None |];
      };
  }
(* award points and reset at the end of each round *)

let valid_move state player card =
  if player.user_id <> state.round_state.current_player then
    raise Exceptions.OutOfOrder
  else if not (List.mem card player.hand) then raise Exceptions.CardNotHeld
  else
    match state.round_state.current_suit with
    | None -> true
    | Some lead ->
        if card.suit = lead then true
        else if List.exists (fun (c : Card.card) -> c.suit = lead) player.hand
        then raise Exceptions.IllegalCard
        else true
(* return true if a move is valid, raise an error according to issue if it is
   not *)

let update_hand players user_id new_hand =
  let players' = Array.copy players in
  players'.(user_id) <- { (players.(user_id)) with hand = new_hand };
  players'
(* update hand by copying array to avoid mutability *)

let make_move state player card =
  (* valid_move state player card; *)
  (* shouldn't be necessary *)
  let rs = state.round_state in
  let table' = Array.copy rs.table in
  let new_hand = List.filter (fun x -> x <> card) player.hand in
  table'.(player.user_id) <- Some card;
  let state' =
    {
      state with
      round_state =
        {
          rs with
          current_suit =
            (if rs.current_suit = None then Some card.suit else rs.current_suit);
          current_player = (rs.current_player + 1) mod 4;
          table = table';
        };
      players = update_hand state.players player.user_id new_hand;
    }
  in
  state'

(* return new state where player has played given card *)

let get_player_view state player =
  {
    pi_current_suit = state.round_state.current_suit;
    pi_trump = state.round_state.trump_suit;
    pi_current_cards = player.hand;
    pi_table = state.round_state.table;
    pi_team = player.team;
    pi_round_number = state.round_state.number;
    pi_current_player = state.round_state.current_player;
  }
(* return useful information for displaying game state to user *)

let get_current_player state =
  let curr_player_id = state.round_state.current_player in
  state.players.(curr_player_id)

let get_user_id player = player.user_id

let get_player_by_id state id =
  let players = Array.copy state.players in
  match id with
  | 0 -> players.(0)
  | 1 -> players.(1)
  | 2 -> players.(2)
  | 3 -> players.(3)
  | _ -> raise Exceptions.MalformedInternalArg

let get_round_winner state =
  let rs = state.round_state in
  if rs.current_player <> rs.first_player then
    raise Exceptions.MalformedInternalArg
  else
    let current_suit =
      match rs.current_suit with
      | None -> raise Exceptions.MalformedInternalArg
      | Some s -> s
    in
    Card.max_card rs.table rs.trump_suit current_suit

let get_team_scores state = state.scores
