type rank =
  | Number of int
  | Jack
  | Queen
  | King
  | Ace

type suit =
  | Hearts
  | Spades
  | Diamonds
  | Clubs

type card = {
  suit : suit;
  rank : rank;
}

val max_card : card option array -> suit -> suit -> int
(* given an array of cards, trump suit, and current suit, returns the index of
   the card with the greatest value *)

val full_deck : card array
(* sorted deck of standard cards *)

val card_of_string : string -> card
(* val string_of_suit_option : suit option -> string *)
(* val string_of_card_option : card option -> string *)
val string_of_suit : suit -> bool -> string
val string_of_rank : rank -> string
val string_of_card : card -> string
