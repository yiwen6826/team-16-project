type hand = Card.card list
(** Rep. Invariant - in whist, the number of cards in a hand in always >=0 and
    <= 13 AF : any [None] represents a lack of a card, whereas a [Some card] is
    an existing card of some valid suit and rank *)

type table = Card.card option array
(* array of four cards that may be in play*)

type player
(* contains state info about player *)

type player_information = {
  pi_current_suit : Card.suit option;
  pi_trump : Card.suit;
  pi_current_cards : hand;
  pi_table : table;
  pi_team : int;
  pi_round_number : int;
  pi_current_player : int;
}

(** Type table (field in the state record) also uses type hand, but its RI is
    the number of cards is >=0 and <= 4 AF: same as the AF for type hand, but
    indices 0,1,2,3 correlate to the card played by players 1,2,3,4,
    respectively, such that a [None] indicates the player has not yet taken
    their turn, and a [Some card] means the player has places this card on the
    table, and {i it no longer exists in their hand} *)

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
val get_current_player : state -> player
val get_user_id : player -> int
val get_player_by_id : state -> int -> player
val get_round_winner : state -> int
val get_team_scores : state -> int * int
val end_round : state -> state