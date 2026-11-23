let rank_value (rank : Card.rank) =
  match rank with
  | Number n -> n
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let highest cards =
  match cards with
  | first :: rest ->
      List.fold_left
        (fun (acc : Card.card) (a : Card.card) ->
          if rank_value acc.rank > rank_value a.rank then acc else a)
        first rest
  | _ -> failwith "highest: empty list"

let lowest cards =
  match cards with
  | first :: rest ->
      List.fold_left
        (fun (acc : Card.card) (a : Card.card) ->
          if rank_value acc.rank < rank_value a.rank then acc else a)
        first rest
  | _ -> failwith "lowest: empty list"

(** [next_move info] is the card the bot plays given the game state [info]. The
    bot follows a fixed, simple strategy and does not consider the cards already
    on the table or which team it is on:
    - If the bot is the first player to go ([info.current_suit] is [None]), it
      plays its highest-ranked non-trump card, or its highest ranked trump if it
      only holds trump cards.
    - If it holds cards of the led suit, it plays the highest-ranked of them.
    - Otherwise, if it holds trump cards, it plays the lowest-ranked trump.
    - Otherwise, it plays its lowest-ranked card of any suit.

    Ranks are ordered 2 < 3 < ... < 10 < Jack < Queen < King < Ace. When several
    cards share the same rank, any of them may be chosen. Requires:
    [info.current_cards] is non-empty. Raises: [Failure] if [info.current_cards]
    is empty. *)
let next_move { Model.current_suit; trump; current_cards; table = _; team = _; round_number = _ }
    =
  match current_suit with
  | None ->
      let non_trump_cards =
        List.filter (fun (card : Card.card) -> card.suit <> trump) current_cards
      in
      if non_trump_cards <> [] then highest non_trump_cards
      else highest current_cards
  | Some s ->
      let current_suit_cards =
        List.filter (fun (card : Card.card) -> card.suit = s) current_cards
      in
      if current_suit_cards <> [] then highest current_suit_cards
      else
        let trump_suit_cards =
          List.filter
            (fun (card : Card.card) -> card.suit = trump)
            current_cards
        in
        if trump_suit_cards <> [] then lowest trump_suit_cards
        else lowest current_cards


let longest hands =
  match hands with
  | first :: rest ->
    List.fold_left (fun acc hand -> if List.length acc > List.length hand then acc else hand) first rest
  | [] -> failwith "longest: empty list" 

let smart_move { Model.current_suit; trump; current_cards; table; team; round_number = _ } =
  match current_suit with
  | None -> (
      let non_trump_cards =
        List.filter (fun (card : Card.card) -> card.suit <> trump) current_cards
      in
      let aces =
        List.filter (fun (card : Card.card) -> card.rank = Ace) non_trump_cards
      in
      match aces with
      | first :: _ -> first
      | [] -> (
          let non_trump_suits =
            List.filter
              (fun (suit : Card.suit) -> suit <> trump)
              ([ Hearts; Spades; Diamonds; Clubs ] : Card.suit list)
          in
          let suit_hands =
            List.map
              (fun suit ->
                List.filter
                  (fun (card : Card.card) -> card.suit = suit)
                  current_cards)
              non_trump_suits
          in
          let non_empty = List.filter (fun hand -> hand <> []) suit_hands in
          match non_empty with
          | [] -> highest current_cards
          | _ -> highest (longest non_empty)))
  | Some v -> 