val inital_get_card : unit -> Card.card
(* gets the player's requested move from cli and turns it into a card *)

val reprompt_card : string -> Card.card
(* if the play enters an invalid card, displays an error message and gets
   another card *)
