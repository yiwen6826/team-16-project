type rank = Number of int | Jack | Queen | King | Ace
type suit = Hearts | Spades | Diamonds | Clubs
type card = {suit : suit; rank : rank}

let full_deck =
  Array.init 52 (fun i ->
      let suit =
        begin match i mod 4 with
        | 0 -> Hearts
        | 1 -> Spades
        | 2 -> Diamonds
        | _ -> Clubs
        end
      in
      let rank =
        begin match (i / 4) + 1 with
        | 1 -> Ace
        | 13 -> King
        | 12 -> Queen
        | 11 -> Jack
        | n -> Number n
        end
      in
      { suit; rank })

let rank_value = function
  | Number n -> n
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let max_card table trump play_suit =
  let strength = function
    | None -> invalid_arg "incomplete trick"
    | Some c ->
        if c.suit = trump then 200 + rank_value c.rank
        else if c.suit = play_suit then 100 + rank_value c.rank
        else 0
  in
  let best = ref 0 in
  for i = 1 to 3 do
    if strength table.(i) > strength table.(!best) then best := i
  done;
  !best

let rank_of_string_opt rank =
  match int_of_string_opt rank with
  | Some r when r >= 2 && r <= 10 -> Some (Number r)
  | None ->
      begin match rank with
      | "A" -> Some Ace
      | "K" -> Some King
      | "Q" -> Some Queen
      | "J" -> Some Jack
      | _ -> None
      end
  | _ -> None

let suit_of_string_opt suit =
  match suit with
  | "H" -> Some Hearts
  | "S" -> Some Spades
  | "D" -> Some Diamonds
  | "C" -> Some Clubs
  | _ -> None


let card_of_string str = 
  begin match rank_of_string_opt (String.sub str 0 1) with
      | Some rank ->
          begin match
            suit_of_string_opt (String.uppercase_ascii (String.sub str 1 1))
          with
          | Some suit -> { rank; suit }
          | None ->
              raise (Exceptions.MalformedCard "Invalid suit, must be H = Hearts, S = Spades, D = Diamonds, C \
                 = Clubs")
          end
      | None ->
          raise (Exceptions.MalformedCard "Invalid rank, must be between 2 and 10, inclusive, or J,Q,K,A")
      end