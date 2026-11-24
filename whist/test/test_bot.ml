open OUnit2
open Whist

(* Shortcut for building a card: [c Hearts King]. *)
let c (suit : Card.suit) (rank : Card.rank) : Card.card = { suit; rank }

(* A real player view from a fresh game. Tests copy it and override the fields
   the bot uses, so the other fields always have valid values. *)
let dummy_view =
  let st = Model.init (Random.State.make [| 42 |]) in
  Model.get_player_view st (Model.get_player_by_id st 0)

(* Builds the bot's input. Leave out [~current_suit] to mean the bot is leading.
   [table] defaults to empty and [my_uid] to player 1, whose partner is player
   3. *)
let info ?current_suit ?(table = [| None; None; None; None |]) ?(my_uid = 1)
    ~trump hand : Model.player_information =
  {
    dummy_view with
    pi_current_suit = current_suit;
    pi_trump = trump;
    pi_current_cards = hand;
    pi_table = table;
    pi_my_uid = my_uid;
  }

let next_move_test name expected i =
  name >:: fun _ -> assert_equal expected (Bot.next_move i)

let smart_test name expected i =
  name >:: fun _ -> assert_equal expected (Bot.smart_move i)

let next_move_tests =
  [
    next_move_test "lead: highest non-trump" (c Clubs King)
      (info ~trump:Spades [ c Hearts (Number 3); c Clubs King; c Spades Ace ]);
    next_move_test "lead: only trump" (c Spades Jack)
      (info ~trump:Spades [ c Spades (Number 2); c Spades Jack ]);
    next_move_test "follow suit: highest" (c Hearts Queen)
      (info ~current_suit:Hearts ~trump:Spades
         [ c Hearts (Number 3); c Hearts Queen; c Clubs Ace ]);
    next_move_test "no led suit: lowest trump" (c Spades (Number 4))
      (info ~current_suit:Hearts ~trump:Spades
         [ c Spades (Number 4); c Spades (Number 9); c Clubs (Number 2) ]);
    next_move_test "no led suit, no trump: lowest" (c Clubs (Number 3))
      (info ~current_suit:Hearts ~trump:Spades
         [ c Diamonds (Number 7); c Clubs (Number 3); c Diamonds King ]);
    next_move_test "led suit is trump" (c Spades (Number 10))
      (info ~current_suit:Spades ~trump:Spades
         [ c Spades (Number 5); c Spades (Number 10); c Hearts Ace ]);
    next_move_test "jack beats 10" (c Hearts Jack)
      (info ~current_suit:Hearts ~trump:Spades
         [ c Hearts Jack; c Hearts (Number 10) ]);
    next_move_test "single card" (c Clubs (Number 6))
      (info ~current_suit:Hearts ~trump:Spades [ c Clubs (Number 6) ]);
    ( "next_move: empty hand raises" >:: fun _ ->
      assert_raises (Failure "lowest: empty list") (fun () ->
          Bot.next_move (info ~current_suit:Hearts ~trump:Spades [])) );
  ]

let smart_tests =
  [
    (* Leading *)
    smart_test "smart lead: non-trump ace" (c Clubs Ace)
      (info ~trump:Spades
         [ c Hearts King; c Clubs Ace; c Spades Ace; c Diamonds (Number 2) ]);
    smart_test "smart lead: highest of longest non-trump suit"
      (c Diamonds (Number 9))
      (info ~trump:Spades
         [
           c Hearts King;
           c Diamonds (Number 3);
           c Diamonds (Number 5);
           c Diamonds (Number 9);
           c Clubs Queen;
           c Spades Ace;
         ]);
    smart_test "smart lead: only trump" (c Spades Jack)
      (info ~trump:Spades [ c Spades (Number 2); c Spades Jack ]);
    (* Following suit *)
    smart_test "smart follow: cheapest winning card" (c Hearts Jack)
      (info ~current_suit:Hearts ~trump:Spades
         ~table:[| Some (c Hearts (Number 9)); None; None; None |]
         [
           c Hearts (Number 5); c Hearts Jack; c Hearts Ace; c Clubs (Number 2);
         ]);
    smart_test "smart follow: can't win, plays lowest" (c Hearts (Number 5))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:[| Some (c Hearts Ace); None; None; None |]
         [ c Hearts (Number 5); c Hearts Jack; c Clubs (Number 2) ]);
    smart_test "smart follow: partner winning, plays lowest"
      (c Hearts (Number 5))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:
           [| Some (c Hearts (Number 4)); None; None; Some (c Hearts King) |]
         [ c Hearts (Number 5); c Hearts Ace ]);
    smart_test "smart follow: opponent trumped, plays lowest"
      (c Hearts (Number 5))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:
           [|
             Some (c Spades (Number 3));
             None;
             Some (c Hearts (Number 7));
             Some (c Hearts (Number 9));
           |]
         [ c Hearts (Number 5); c Hearts Ace ]);
    smart_test "smart follow: trump led, cheapest winning trump" (c Spades Jack)
      (info ~current_suit:Spades ~trump:Spades
         ~table:[| Some (c Spades (Number 9)); None; None; None |]
         [ c Spades (Number 5); c Spades Jack; c Hearts Ace ]);
    (* Out of the led suit *)
    smart_test "smart void: lowest winning trump" (c Spades (Number 4))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:[| Some (c Hearts (Number 10)); None; None; None |]
         [ c Spades (Number 4); c Spades (Number 9); c Clubs (Number 2) ]);
    smart_test "smart void: partner winning, discards lowest non-trump"
      (c Clubs (Number 2))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:
           [| Some (c Hearts (Number 5)); None; None; Some (c Hearts Ace) |]
         [ c Spades (Number 4); c Clubs (Number 2); c Diamonds (Number 8) ]);
    smart_test "smart void: opponent's trump too high, discards"
      (c Clubs (Number 3))
      (info ~current_suit:Hearts ~trump:Spades
         ~table:
           [|
             Some (c Spades Queen);
             None;
             Some (c Hearts (Number 6));
             Some (c Hearts (Number 8));
           |]
         [
           c Spades (Number 4);
           c Spades (Number 9);
           c Clubs (Number 3);
           c Diamonds (Number 7);
         ]);
    ( "smart_move: empty hand raises" >:: fun _ ->
      assert_raises (Failure "highest: empty list") (fun () ->
          Bot.smart_move (info ~trump:Spades [])) );
  ]

let suite = "bot tests" >::: next_move_tests @ smart_tests
let () = run_test_tt_main suite
