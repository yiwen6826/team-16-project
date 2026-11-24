(** [rank_value rank] is the numeric strength of [rank]: [n] for [Number n],
    then 11 for Jack, 12 for Queen, 13 for King and 14 for Ace. *)
let rank_value (rank : Card.rank) =
  match rank with
  | Number n -> n
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

(** [highest cards] is the card in [cards] with the highest rank. If several
    cards share the highest rank, any one of them may be returned. Raises:
    [Failure] if [cards] is empty. *)
let highest cards =
  match cards with
  | first :: rest ->
      List.fold_left
        (fun (acc : Card.card) (a : Card.card) ->
          if rank_value acc.rank > rank_value a.rank then acc else a)
        first rest
  | _ -> failwith "highest: empty list"

(** [lowest cards] is the card in [cards] with the lowest rank. If several cards
    share the lowest rank, any one of them may be returned. Raises: [Failure] if
    [cards] is empty. *)
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
    - If the bot is the first player to go ([info.pi_current_suit] is [None]),
      it plays its highest-ranked non-trump card, or its highest-ranked trump if
      it only holds trump cards.
    - If it holds cards of the led suit, it plays the highest-ranked of them.
    - Otherwise, if it holds trump cards, it plays the lowest-ranked trump.
    - Otherwise, it plays its lowest-ranked card of any suit.

    Ranks are ordered 2 < 3 < ... < 10 < Jack < Queen < King < Ace. When several
    cards share the same rank, any of them may be chosen. Requires:
    [info.pi_current_cards] is non-empty. Raises: [Failure] if
    [info.pi_current_cards] is empty. *)
let next_move { Model.pi_current_suit; pi_trump; pi_current_cards; _ } =
  Unix.sleep 1;
  match pi_current_suit with
  | None ->
      let non_trump_cards =
        List.filter
          (fun (card : Card.card) -> card.suit <> pi_trump)
          pi_current_cards
      in
      if non_trump_cards <> [] then highest non_trump_cards
      else highest pi_current_cards
  | Some s ->
      let current_suit_cards =
        List.filter (fun (card : Card.card) -> card.suit = s) pi_current_cards
      in
      if current_suit_cards <> [] then highest current_suit_cards
      else
        let trump_suit_cards =
          List.filter
            (fun (card : Card.card) -> card.suit = pi_trump)
            pi_current_cards
        in
        if trump_suit_cards <> [] then lowest trump_suit_cards
        else lowest pi_current_cards

(** [longest hands] is the longest list in [hands]. If several lists share the
    greatest length, the last one is returned. Raises: [Failure] if [hands] is
    empty. *)
let longest hands =
  match hands with
  | first :: rest ->
      List.fold_left
        (fun acc hand ->
          if List.length acc > List.length hand then acc else hand)
        first rest
  | [] -> failwith "longest: empty list"

(** [beats trump c w] is [true] if card [c] would win against card [w] when
    [trump] is the trump suit: either [c] has the same suit as [w] and a higher
    rank, or [c] is a trump and [w] is not. A card of a different non-trump suit
    never beats [w]. *)
let beats trump (c : Card.card) (w : Card.card) =
  (c.suit = w.suit && rank_value c.rank > rank_value w.rank)
  || (c.suit = trump && w.suit <> trump)

(** [current_winner trump led played] is the card currently winning a trick in
    which [played] are the cards played so far, [led] is the led suit, and
    [trump] is the trump suit. It is the highest trump in [played] if any trump
    has been played, and otherwise the highest card of suit [led]. Requires:
    [played] contains at least one card of suit [led] or of suit [trump]. *)
let current_winner trump led (played : Card.card list) =
  let trumps = List.filter (fun (c : Card.card) -> c.suit = trump) played in
  if trumps <> [] then highest trumps
  else
    let led_cards = List.filter (fun (c : Card.card) -> c.suit = led) played in
    highest led_cards

(** [partner_winning my_id table curr_winner] is [true] if the partner of player
    [my_id] has played [curr_winner] on [table], meaning the partner is
    currently winning the trick. The partner is the player with id
    [(my_id + 2) mod 4]. It is [false] if the partner has not played yet.
    Requires: [my_id] is between 0 and 3. *)
let partner_winning my_id (table : Model.table) curr_winner =
  let partner_id = (my_id + 2) mod 4 in
  match table.(partner_id) with
  | None -> false
  | Some card -> card = curr_winner

(** [smart_move info] is the card a stronger bot plays given the game state
    [info]. Unlike [next_move], it looks at the cards already on the table and
    avoids beating its partner (the player whose id is
    [(info.pi_my_uid + 2) mod 4]):
    - If the bot is leading ([info.pi_current_suit] is [None]), it plays a
      non-trump Ace if it holds one. Otherwise it plays the highest card of its
      longest non-trump suit, or its highest trump if it holds only trump.
    - If it holds cards of the led suit and its partner is currently winning the
      trick, it plays its lowest card of the led suit.
    - If it holds cards of the led suit and its partner is not winning, it plays
      its lowest card of that suit that beats the current winning card, or its
      lowest card of that suit if none can win.
    - If it holds no cards of the led suit and its partner is not winning, it
      plays its lowest trump that beats the current winning card, if any.
    - Otherwise, it plays its lowest non-trump card, or its lowest trump if it
      holds only trump.

    The current winning card is the highest trump on the table, or the highest
    card of the led suit if no trump has been played. Ranks are ordered 2 < 3 <
    ... < 10 < Jack < Queen < King < Ace, and ties may be broken either way.
    Requires: [info.pi_current_cards] is non-empty, and if
    [info.pi_current_suit] is [Some s], [info.pi_table] holds at least one card
    of suit [s]. Raises: [Failure] if [info.pi_current_cards] is empty. *)
let smart_move
    {
      Model.pi_current_suit = current_suit;
      pi_trump = trump;
      pi_current_cards = current_cards;
      pi_table = table;
      pi_my_uid = my_uid;
      _;
    } =
  Unix.sleep 1;
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
      let played = List.filter_map Fun.id (Array.to_list table) in
      let winner = current_winner trump v played in
      let partner_wins = partner_winning my_uid table winner in
      let led_suit_cards =
        List.filter (fun (c : Card.card) -> c.suit = v) current_cards
      in
      if led_suit_cards <> [] then
        if partner_wins then lowest led_suit_cards
        else
          let beats_winner =
            List.filter
              (fun (c : Card.card) -> beats trump c winner)
              led_suit_cards
          in
          if beats_winner <> [] then lowest beats_winner
          else lowest led_suit_cards
      else
        let beats_winner_trumps =
          List.filter
            (fun (c : Card.card) -> c.suit = trump && beats trump c winner)
            current_cards
        in
        if (not partner_wins) && beats_winner_trumps <> [] then
          lowest beats_winner_trumps
        else
          let non_trump_cards =
            List.filter (fun (c : Card.card) -> c.suit <> trump) current_cards
          in
          if non_trump_cards <> [] then lowest non_trump_cards
          else lowest current_cards
