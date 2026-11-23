val get_card : unit -> Card.card
(**[get_card ()] returns a card based on user terminal input. Does not check if
   the card is valid.*)
val inital_get_card : unit -> Card.card
(* gets the player's requested move from cli and turns it into a card *)

val reprompt_card : string -> Card.card
(* if the play enters an invalid card, displays an error message and gets
   another card *)
