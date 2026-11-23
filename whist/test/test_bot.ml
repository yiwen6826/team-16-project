open OUnit2
open Whist

(* Shortcut for building a card: [c Hearts King]. *)
let c (suit : Card.suit) (rank : Card.rank) : Card.card = { suit; rank }

(* A real table from a fresh game. The bot ignores it, so any table works. *)
let dummy_table =
  let st = Model.init (Random.State.make [| 42 |]) in
  (Model.get_player_view st (Model.get_player_by_id st 0)).table

(* Builds the bot's input. Leave out [~current_suit] to mean the bot is leading.
   The bot ignores [table], [team] and [round_number]. *)
let info ?current_suit ~trump hand : Model.player_information =
  {
    current_suit;
    trump;
    current_cards = hand;
    table = dummy_table;
    team = 0;
    round_number = 0;
  }

let bot_test name expected i =
  name >:: fun _ -> assert_equal expected (Bot.next_move i)

let bot_tests =
  [
    bot_test "lead: highest non-trump" (c Clubs King)
      (info ~trump:Spades [ c Hearts (Number 3); c Clubs King; c Spades Ace ]);
    bot_test "lead: only trump" (c Spades Jack)
      (info ~trump:Spades [ c Spades (Number 2); c Spades Jack ]);
    bot_test "follow suit: highest" (c Hearts Queen)
      (info ~current_suit:Hearts ~trump:Spades
         [ c Hearts (Number 3); c Hearts Queen; c Clubs Ace ]);
    bot_test "no led suit: lowest trump" (c Spades (Number 4))
      (info ~current_suit:Hearts ~trump:Spades
         [ c Spades (Number 4); c Spades (Number 9); c Clubs (Number 2) ]);
    bot_test "no led suit, no trump: lowest" (c Clubs (Number 3))
      (info ~current_suit:Hearts ~trump:Spades
         [ c Diamonds (Number 7); c Clubs (Number 3); c Diamonds King ]);
    bot_test "led suit is trump" (c Spades (Number 10))
      (info ~current_suit:Spades ~trump:Spades
         [ c Spades (Number 5); c Spades (Number 10); c Hearts Ace ]);
    bot_test "jack beats 10" (c Hearts Jack)
      (info ~current_suit:Hearts ~trump:Spades
         [ c Hearts Jack; c Hearts (Number 10) ]);
    bot_test "single card" (c Clubs (Number 6))
      (info ~current_suit:Hearts ~trump:Spades [ c Clubs (Number 6) ]);
    ( "empty hand raises" >:: fun _ ->
      assert_raises (Failure "lowest: empty list") (fun () ->
          Bot.next_move (info ~current_suit:Hearts ~trump:Spades [])) );
  ]

let suite = "bot tests" >::: bot_tests
let () = run_test_tt_main suite
