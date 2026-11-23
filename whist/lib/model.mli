type hand = Card.card list
(** Rep. Invariant - in whist, the number of cards in a hand in always >=0 and
    <= 13 AF : any [None] represents a lack of a card, whereas a [Some card] is
    an existing card of some valid suit and rank *)

type table
(* array of four cards that may be in play*)

type player
(* contains state info about player *)

type player_information = {
  current_suit : Card.suit option;
  trump : Card.suit;
  current_cards : hand;
  table : table;
  team : int;
  round_number : int;
}
(* information for displaying view to player *)

type state
(* state information about game *)

val init : Random.State.t -> state
(* init new game *)

val new_round : state -> state
(* award points and reset at the end of round. precondition: all four players
   have played their cards *)

val valid_move : state -> player -> Card.card -> bool
(* checks if move is valid and raises helpful error if it is not *)

val make_move : state -> player -> Card.card -> state
(* returns new state after player plays the card. precondition: valid move has
   already been checked and returned true *)
   
val get_player_view : state -> player -> player_information
(* returns appropriate player information *)

val get_current_player : state -> player
