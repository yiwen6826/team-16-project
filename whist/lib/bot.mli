val next_move : Model.player_information -> Card.card
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

val smart_move : Model.player_information -> Card.card
(** [smart_move info] is the card a stronger bot plays given the game state
    [info]. Unlike [next_move], it looks at the cards already on the table and
    avoids beating its partner (the player whose id is
    [(info.my_uid + 2) mod 4]):
    - If the bot is leading ([info.current_suit] is [None]), it plays a
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
    Requires: [info.current_cards] is non-empty, and if [info.current_suit] is
    [Some s], the table holds at least one card of suit [s]. Raises: [Failure]
    if [info.current_cards] is empty. *)
