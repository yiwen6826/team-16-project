(** Terminal display for single-player Whist.

    The controller passes in what the human player can see and gets back text
    to print. The controller reads input, checks moves with the model, and
    supplies the game result. *)

(** How many tricks each team has won. *)
type score = { your_team : int; opponents : int }

(** What the player sees when it's their turn. Pass card and suit names as
    strings such as ["Ace of Spades"] and ["Spades"]. *)
type turn = {
  trick_number : int;  (** The current trick number, from 1 to 13. *)
  trump_suit : string;
  score : score;
  hand : (int * string) list;
      (** Each entry pairs a slot number with a card name. Number slots from
          1, skip empty slots, and keep the original numbers after a play. *)
  table : (string * string) list;
      (** Each entry gives the player and the card they played this trick.
          Use an empty list before anyone has played. *)
}

(** Whether the human player's team won or lost the game. *)
type outcome = Won | Lost

val render_turn : turn -> string
(** [render_turn turn] shows the trick number, trump suit, score, table, and
    numbered hand, then asks the player to enter a card number. It lists the
    hand and table entries in the order they were passed in. *)

val render_invalid_choice : reason:string -> string
(** [render_invalid_choice ~reason] tells the player why the controller
    rejected their choice. *)

val render_result : outcome:outcome -> score:score -> string
(** [render_result ~outcome ~score] tells the player whether their team won
    and how many tricks each team took. *)
