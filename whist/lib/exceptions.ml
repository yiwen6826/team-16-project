exception CardNotHeld
(* raised if the player tries to play a card they don't own *)

exception IllegalCard
(* raised if the player tries an illegal move (like playing a trump when they have a card of the current suit) *)

exception OutOfOrder
(* raised if the player tries to play when it is not their turn *)