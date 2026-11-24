open OUnit2
open Whist

(* ---------- helpers ---------- *)

let seed n = Random.State.make [| n |]
let new_game n = Model.init (seed n)
let player s id = Model.get_player_by_id s id
let view s id = Model.get_player_view s (player s id)
let hand_of s id = (view s id).pi_current_cards
let suit_of (c : Card.card) = c.Card.suit

let rec find_state pred n =
  if n > 20000 then failwith "no suitable seed found"
  else
    let s = new_game n in
    if pred s then s else find_state pred (n + 1)

(* first card in a player's hand that valid_move accepts *)
let legal_card s p =
  List.find
    (fun c -> try Model.valid_move s p c with _ -> false)
    (hand_of s (Model.get_user_id p))

let play_next s =
  let p = Model.get_current_player s in
  Model.make_move s p (legal_card s p)

let play_full_round s = s |> play_next |> play_next |> play_next |> play_next
let count_some t = Array.fold_left (fun n x -> if x = None then n else n + 1) 0 t

(* ---------- init ---------- *)

let init_tests =
  [
    ( "each player has 13 cards" >:: fun _ ->
      let s = new_game 1 in
      for i = 0 to 3 do
        assert_equal 13 (List.length (hand_of s i))
      done );
    ( "all 52 dealt cards are distinct" >:: fun _ ->
      let s = new_game 2 in
      let all = List.concat (List.init 4 (hand_of s)) in
      assert_equal 52 (List.length all);
      assert_equal 52 (List.length (List.sort_uniq compare all)) );
    ( "user ids are 0..3" >:: fun _ ->
      let s = new_game 3 in
      for i = 0 to 3 do
        assert_equal i (Model.get_user_id (player s i))
      done );
    ( "teams alternate 0,1,0,1" >:: fun _ ->
      let s = new_game 3 in
      let teams = List.init 4 (fun i -> (view s i).pi_team) in
      assert_equal [ 0; 1; 0; 1 ] teams );
    ( "initial scores are 0-0" >:: fun _ ->
      assert_equal (0, 0) (Model.get_team_scores (new_game 4)) );
    ( "initial round state" >:: fun _ ->
      let v = view (new_game 5) 0 in
      assert_equal 0 v.pi_round_number;
      assert_equal 0 v.pi_current_player;
      assert_equal None v.pi_current_suit;
      assert_equal 0 (count_some v.pi_table);
      assert_equal 4 (Array.length v.pi_table) );
    ( "same seed gives same deal" >:: fun _ ->
      let a = new_game 6 and b = new_game 6 in
      for i = 0 to 3 do
        assert_equal (hand_of a i) (hand_of b i)
      done;
      assert_equal (view a 0).pi_trump (view b 0).pi_trump );
    ( "different seeds give different deals" >:: fun _ ->
      assert_bool "hands should differ"
        (hand_of (new_game 7) 0 <> hand_of (new_game 8) 0) );
  ]

(* ---------- getters ---------- *)

let getter_tests =
  [
    ( "get_current_player is player 0 at start" >:: fun _ ->
      assert_equal 0 (Model.get_user_id (Model.get_current_player (new_game 9))) );
    ( "get_player_by_id rejects out of range ids" >:: fun _ ->
      let s = new_game 9 in
      assert_raises Exceptions.MalformedInternalArg (fun () ->
          Model.get_player_by_id s 4);
      assert_raises Exceptions.MalformedInternalArg (fun () ->
          Model.get_player_by_id s (-1)) );
    ( "get_player_view reflects player's own data" >:: fun _ ->
      let s = new_game 10 in
      let v = view s 2 in
      assert_equal 2 v.pi_my_uid;
      assert_equal 0 v.pi_team;
      assert_equal (hand_of s 2) v.pi_current_cards );
  ]

(* ---------- valid_move ---------- *)

let valid_move_tests =
  [
    ( "leader may play any card in hand" >:: fun _ ->
      let s = new_game 11 in
      let p = player s 0 in
      List.iter
        (fun c -> assert_bool "valid" (Model.valid_move s p c))
        (hand_of s 0) );
    ( "wrong player raises OutOfOrder" >:: fun _ ->
      let s = new_game 11 in
      let p1 = player s 1 in
      let c = List.hd (hand_of s 1) in
      assert_raises Exceptions.OutOfOrder (fun () -> Model.valid_move s p1 c) );
    ( "card not in hand raises CardNotHeld" >:: fun _ ->
      let s = new_game 11 in
      let p0 = player s 0 in
      let other = List.hd (hand_of s 1) in
      assert_raises Exceptions.CardNotHeld (fun () -> Model.valid_move s p0 other) );
    ( "must follow suit when able (IllegalCard)" >:: fun _ ->
      let pred s =
        let lead = suit_of (List.hd (hand_of s 0)) in
        let h1 = hand_of s 1 in
        List.exists (fun c -> suit_of c = lead) h1
        && List.exists (fun c -> suit_of c <> lead) h1
      in
      let s = find_state pred 0 in
      let p0 = player s 0 in
      let lead_card = List.hd (hand_of s 0) in
      let s' = Model.make_move s p0 lead_card in
      let p1 = Model.get_current_player s' in
      let off = List.find (fun c -> suit_of c <> suit_of lead_card) (hand_of s' 1) in
      assert_raises Exceptions.IllegalCard (fun () -> Model.valid_move s' p1 off) );
    ( "following suit is accepted" >:: fun _ ->
      let pred s =
        let lead = suit_of (List.hd (hand_of s 0)) in
        List.exists (fun c -> suit_of c = lead) (hand_of s 1)
      in
      let s = find_state pred 0 in
      let lead_card = List.hd (hand_of s 0) in
      let s' = Model.make_move s (player s 0) lead_card in
      let p1 = Model.get_current_player s' in
      let follow =
        List.find (fun c -> suit_of c = suit_of lead_card) (hand_of s' 1)
      in
      assert_bool "valid" (Model.valid_move s' p1 follow) );
    ( "any card allowed when void in lead suit" >:: fun _ ->
      let pred s =
        let lead = suit_of (List.hd (hand_of s 0)) in
        not (List.exists (fun c -> suit_of c = lead) (hand_of s 1))
      in
      let s = find_state pred 0 in
      let s' = Model.make_move s (player s 0) (List.hd (hand_of s 0)) in
      let p1 = Model.get_current_player s' in
      List.iter
        (fun c -> assert_bool "valid" (Model.valid_move s' p1 c))
        (hand_of s' 1) );
  ]

(* ---------- make_move ---------- *)

let make_move_tests =
  [
    ( "played card is removed from hand" >:: fun _ ->
      let s = new_game 12 in
      let p = player s 0 in
      let c = List.hd (hand_of s 0) in
      let s' = Model.make_move s p c in
      assert_equal 12 (List.length (hand_of s' 0));
      assert_bool "card gone" (not (List.mem c (hand_of s' 0))) );
    ( "remaining cards are preserved" >:: fun _ ->
      let s = new_game 12 in
      let c = List.hd (hand_of s 0) in
      let s' = Model.make_move s (player s 0) c in
      assert_equal
        (List.filter (fun x -> x <> c) (hand_of s 0))
        (hand_of s' 0) );
    ( "played card appears on table at player's index" >:: fun _ ->
      let s = new_game 13 in
      let c = List.hd (hand_of s 0) in
      let s' = Model.make_move s (player s 0) c in
      let t = (view s' 0).pi_table in
      assert_equal (Some c) t.(0);
      assert_equal 1 (count_some t) );
    ( "other players' hands are untouched" >:: fun _ ->
      let s = new_game 13 in
      let s' = Model.make_move s (player s 0) (List.hd (hand_of s 0)) in
      for i = 1 to 3 do
        assert_equal (hand_of s i) (hand_of s' i)
      done );
    ( "first card sets the current suit" >:: fun _ ->
      let s = new_game 14 in
      let c = List.hd (hand_of s 0) in
      let s' = Model.make_move s (player s 0) c in
      assert_equal (Some (suit_of c)) (view s' 0).pi_current_suit );
    ( "current suit not overwritten by later cards" >:: fun _ ->
      let s = new_game 14 in
      let c = List.hd (hand_of s 0) in
      let s1 = Model.make_move s (player s 0) c in
      let s2 = play_next s1 in
      assert_equal (Some (suit_of c)) (view s2 0).pi_current_suit );
    ( "turn advances to next player" >:: fun _ ->
      let s = new_game 15 in
      let s' = Model.make_move s (player s 0) (List.hd (hand_of s 0)) in
      assert_equal 1 (Model.get_user_id (Model.get_current_player s')) );
    ( "turn wraps from player 3 to 0" >:: fun _ ->
      let s = new_game 15 in
      let s' = play_full_round s in
      assert_equal 0 (Model.get_user_id (Model.get_current_player s')) );
    ( "original state is not mutated" >:: fun _ ->
      let s = new_game 16 in
      let before_hand = hand_of s 0 in
      let _ = Model.make_move s (player s 0) (List.hd before_hand) in
      assert_equal before_hand (hand_of s 0);
      assert_equal 0 (count_some (view s 0).pi_table);
      assert_equal None (view s 0).pi_current_suit );
    ( "after full round, four cards on table and hands have 12" >:: fun _ ->
      let s' = play_full_round (new_game 17) in
      assert_equal 4 (count_some (view s' 0).pi_table);
      for i = 0 to 3 do
        assert_equal 12 (List.length (hand_of s' i))
      done );
  ]

(* ---------- rounds, winners, scoring ---------- *)

let round_tests =
  [
    ( "get_round_winner raises mid-round" >:: fun _ ->
      let s = new_game 18 in
      let s' = Model.make_move s (player s 0) (List.hd (hand_of s 0)) in
      assert_raises Exceptions.MalformedInternalArg (fun () ->
          Model.get_round_winner s') );
    ( "get_round_winner raises before any card is played" >:: fun _ ->
      (* current_player = first_player but no suit is set *)
      assert_raises Exceptions.MalformedInternalArg (fun () ->
          Model.get_round_winner (new_game 18)) );
    ( "get_round_winner returns a valid player id after full round"
    >:: fun _ ->
      let w = Model.get_round_winner (play_full_round (new_game 19)) in
      assert_bool "in range" (w >= 0 && w <= 3) );
    ( "end_round raises when no suit has been led" >:: fun _ ->
      assert_raises Exceptions.MalformedInternalArg (fun () ->
          Model.end_round (new_game 20)) );
    ( "end_round awards exactly one point to winner's team" >:: fun _ ->
      let s = play_full_round (new_game 21) in
      let w = Model.get_round_winner s in
      let a, b = Model.get_team_scores (Model.end_round s) in
      if w = 0 || w = 2 then assert_equal (1, 0) (a, b)
      else assert_equal (0, 1) (a, b) );
    ( "new_round raises when no suit has been led" >:: fun _ ->
      assert_raises (Invalid_argument "new round triggered out of order")
        (fun () -> Model.new_round (new_game 22)) );
    ( "new_round resets table and suit, bumps round number" >:: fun _ ->
      let s = play_full_round (new_game 23) in
      let v = view (Model.new_round s) 0 in
      assert_equal 0 (count_some v.pi_table);
      assert_equal None v.pi_current_suit;
      assert_equal 1 v.pi_round_number );
    ( "new_round makes the winner lead" >:: fun _ ->
      let s = play_full_round (new_game 23) in
      let w = Model.get_round_winner s in
      let s' = Model.new_round s in
      assert_equal w (Model.get_user_id (Model.get_current_player s')) );
    ( "new_round does not change hands or scores" >:: fun _ ->
      let s = play_full_round (new_game 24) in
      let s' = Model.new_round s in
      for i = 0 to 3 do
        assert_equal (hand_of s i) (hand_of s' i)
      done;
      assert_equal (Model.get_team_scores s) (Model.get_team_scores s') );
    ( "play multiple rounds: scores sum to rounds played" >:: fun _ ->
      let rec go s n =
        if n = 0 then s else go (Model.new_round (Model.end_round (play_full_round s))) (n - 1)
      in
      let s = go (new_game 25) 13 in
      let a, b = Model.get_team_scores s in
      assert_equal 13 (a + b);
      for i = 0 to 3 do
        assert_equal 0 (List.length (hand_of s i))
      done );
  ]

let suite =
  "game tests"
  >::: init_tests @ getter_tests @ valid_move_tests @ make_move_tests
       @ round_tests

let () = run_test_tt_main suite
