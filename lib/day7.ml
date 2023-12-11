open IntMap

type hand =
  { hand_val : int
  ; cards_as_ints : int list
  ; bid : int
  }

type accumulator =
  { total : int
  ; rank : int
  }

let increment_count_for_key map inc_by key =
  IntMap.add
    key
    (match IntMap.find_opt key map with
     | Some v -> v + inc_by
     | None -> inc_by)
    map
;;

let sorted_card_map_values card_map : int list =
  card_map
  |> IntMap.bindings
  |> List.sort (fun (_, v1) (_, v2) -> compare v2 v1)
  |> List.map snd
;;

let compare_hand hand1 hand2 =
  let rec compare_cards cards1 cards2 =
    match cards1, cards2 with
    | [], [] -> 0
    | a :: rest1, b :: rest2 ->
      (match a, b with
       | a, b when a < b -> -1
       | a, b when a > b -> 1
       | a, b when a = b -> compare_cards rest1 rest2
       | _ -> failwith "invalid card comparison")
    | _ -> failwith "invalid card comparison"
  in
  match hand1.hand_val, hand2.hand_val with
  | a, b when a > b -> 1
  | a, b when a < b -> -1
  | a, b when a = b -> compare_cards hand1.cards_as_ints hand2.cards_as_ints
  | _ -> 0
;;

let card_value jack_value card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> jack_value
  | 'T' -> 10
  | v -> int_of_string (String.make 1 v)
;;

let key_of_max_value int_map =
  match
    IntMap.bindings int_map
    |> List.fold_left
         (fun acc (key, value) ->
           if key <> 1 && value > snd acc then key, value else acc)
         (0, min_int)
  with
  | _, v when v = min_int -> None
  | k, _ -> Some k
;;

let hand_value card_counts =
  let map_to_sort =
    if IntMap.mem 1 card_counts
    then
      if IntMap.find 1 card_counts = 5 (* if 5 jokers *)
      then card_counts
      else (
        let val_to_bump =
          match key_of_max_value card_counts with
          | Some v -> v
          | None -> failwith "uh oh"
        in
        let bump_by = IntMap.find 1 card_counts in
        let new_map = increment_count_for_key card_counts bump_by val_to_bump in
        IntMap.remove 1 new_map)
    else card_counts
  in
  match sorted_card_map_values map_to_sort with
  | [ 5 ] -> 7
  | [ 4; 1 ] -> 6
  | [ 3; 2 ] -> 5
  | [ 3; 1; 1 ] -> 4
  | [ 2; 2; 1 ] -> 3
  | [ 2; 1; 1; 1 ] -> 2
  | [ 1; 1; 1; 1; 1 ] -> 1
  | _ -> failwith "invalid hand"
;;

let get_card_value_counts jack_value cards =
  let rec get_card_value_counts' cards acc =
    match cards with
    | [] -> acc
    | cur :: rest ->
      cur
      |> card_value jack_value
      |> increment_count_for_key acc 1
      |> get_card_value_counts' rest
  in
  get_card_value_counts' (Util.explode cards) IntMap.empty
;;

let calulate_card_values jack_value line =
  match String.split_on_char ' ' line with
  | [ cards; bid ] ->
    { cards_as_ints = List.map (card_value jack_value) (Util.explode cards)
    ; hand_val = hand_value (get_card_value_counts jack_value cards)
    ; bid = int_of_string bid
    }
  | _ -> failwith "invalid line"
;;

let calculate_winnings hands =
  let result =
    hands
    |> List.sort compare_hand
    |> List.fold_left
         (fun acc hand ->
           { total = acc.total + (hand.bid * acc.rank); rank = acc.rank + 1 })
         { total = 0; rank = 1 }
  in
  result.total
;;

let part1 =
  let jack_value = 11 in
  "input/7.txt"
  |> File.read_lines
  |> List.map (calulate_card_values jack_value)
  |> calculate_winnings
;;

let part2 =
  let jack_value = 1 in
  "input/7.txt"
  |> File.read_lines
  |> List.map (calulate_card_values jack_value)
  |> calculate_winnings
;;
