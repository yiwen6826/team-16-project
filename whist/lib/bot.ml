
let next_move { Model.current_suit; trump; current_cards; table; team } = 
  let current_suit_cards = List.filter (fun (card : Card.card) -> card.suit = current_suit) current_cards in
  if current_suit_cards <> [] then highest current_suit_cards
  else 
    let trump_suit_cards = List.filter (fun (card : Card.card) -> card.suit = trump) current_cards in
    if trump_suit_cards <> [] then lowest trump_suit_cards 
    else 
      lowest current_cards



  (**makes a list of all cards it has of the current_suit in current_cards.
  has a helper function that gets the highest worth one. returns it. 
  if theres no cards of current suit in current cards, it needs to check if it has any cards of the trump suit. 
    if it does, it will pick the lowest valued trump card and return it. if it does not, it will pick the lowest valued card that it has of any suit.*)


(*  type hand = Card.card list  *)
(*  type card = {suit : suit; rank : rank}  *)
