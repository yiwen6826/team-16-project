Random.self_init ();

(** Invariant - Number can only hold int values of 2-10 *)
type rank = Number of int | Jack | Queen | King | Ace
type suit = Hearts | Spades | Diamonds | Clubs
type card = {suit : suit; rank : rank}

(** Rep. Invariant - in whist, the number of cards in a hand in always >=0 and <= 13
AF : any [None] represents a lack of a card, whereas a [Some card] is an existing card of some valid suit and rank *)
type hand = (card option) array (** card list *)

type player = {
  hand : hand;
  user_id : string; (** should this signify real user vs. bot? or do we need another *)
}

(** Type table (field in the state record) uses type card array, 
but its RI is the number of cards is >=0 and <= 4 
AF: same as the AF for type hand, but indices 0,1,2,3 correlate to the card played by players 1,2,3,4, respectively, 
such that a [None] indicates the player has not yet taken their turn, and a [Some card] means 
the player has places this card on the table, and {i it no longer exists in their hand} *)
type table = card array

type state = {
  player1 : player;
  player2 : player;
  player3 : player;
  player4 : player;
  table : table
}

let full_deck = Array.init 52 (fun i -> let suit = begin 
  match i mod 4 with
  | 0 -> Hearts
  | 1 -> Spades
  | 2 -> Diamonds
  | _ -> Clubs
  end
in let rank = begin 
  match (i / 4)+1 with 
  | 1 -> Ace 
  | 13 -> King
  | 12 -> Queen
  | 11 -> Jack
  | n -> Number n
  end
in {suit = suit; rank = rank}
  )

let to_option arr = Array.map (fun card -> if card.rank = Number 0 then None else Some card) arr

(** Random is initialized at the top of this file, 
but we may be allowing to be seeded in gameplay *)
let init () = let deck = Array.copy full_deck in
  Array.shuffle ~rand:Random.int deck;
  { 
    player1 = {hand = to_option (Array.sub deck 0 12); user_id = "player_1"}; 
    player2 = {hand = to_option (Array.sub deck 13 25); user_id = "player_2"};
    player3 =  {hand = to_option (Array.sub deck 26 38); user_id = "player_3"};
    player4 = {hand = to_option (Array.sub deck 39 51); user_id = "player_4"};
    table = [||]
  }