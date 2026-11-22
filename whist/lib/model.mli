(** Invariant - Number can only hold int values of 2-10 *)
type rank = Number of int | Jack | Queen | King | Ace
type suit = Hearts | Spades | Diamonds | Clubs
type card = {suit : suit; rank : rank}

(** Rep. Invariant - in whist, the number of cards in a hand in always >=0 and <= 13
AF : any [None] represents a lack of a card, whereas a [Some card] is an existing card of some valid suit and rank *)
type hand = (card option) array

(** Type table (field in the state record) also uses type hand, 
but its RI is the number of cards is >=0 and <= 4 
AF: same as the AF for type hand, but indices 0,1,2,3 correlate to the card played by players 1,2,3,4, respectively, 
such that a [None] indicates the player has not yet taken their turn, and a [Some card] means 
the player has places this card on the table, and {i it no longer exists in their hand} *)
type table = hand

type state = {
  player1 : hand;
  player2 : hand;
  player3 : hand;
  player4 : hand;
  table : table
}

val init : unit -> state
val full_deck : card array