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