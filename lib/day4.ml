open IntSet

module CardMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

type card =
  { count : int
  ; value : int
  }

type card_map = card CardMap.t

let derive_card_number game =
  match Util.split_on_whitespace game with
  | [ _; card_id ] -> int_of_string card_id
  | _ -> failwith "invalid card"
;;

let make_set_from_string s =
  s
  |> String.trim
  |> Util.split_on_whitespace
  |> List.map String.trim
  |> List.map int_of_string
  |> List.fold_left (fun set num -> IntSet.add num set) IntSet.empty
;;

let rec increment_card_counts card_num left_to_increment increment_by card_map =
  match left_to_increment with
  | 0 -> card_map
  | _ ->
    CardMap.add
      card_num
      (match CardMap.find_opt card_num card_map with
       | Some v -> { v with count = v.count + increment_by }
       | None -> { count = increment_by; value = 0 })
      card_map
    |> increment_card_counts (card_num + 1) (left_to_increment - 1) increment_by
;;

let set_card_value card_num value card_map =
  let new_card_value =
    match CardMap.find_opt card_num card_map with
    | Some v -> { count = v.count + 1; value }
    | None -> { count = 1; value }
  in
  CardMap.add card_num new_card_value card_map, new_card_value
;;

let process_card_results card_results card_num max_row (acc : card_map) =
  match Util.trim_and_split_on_char '|' card_results with
  | [ winning_nums; my_nums ] ->
    let number_of_winning_nums =
      IntSet.inter
        (make_set_from_string winning_nums)
        (make_set_from_string my_nums)
      |> IntSet.cardinal
    in
    let card_value =
      number_of_winning_nums
      |> (fun x -> x - 1)
      |> float_of_int
      |> (fun x -> 2.0 ** x)
      |> int_of_float
    in
    let new_acc, new_card_value = set_card_value card_num card_value acc in
    if new_card_value.count <> 0 && card_num <= max_row
    then
      increment_card_counts
        (card_num + 1)
        number_of_winning_nums
        new_card_value.count
        new_acc
    else new_acc
  | _ -> failwith "invalid results"
;;

let rec process_lines_acc lines max_row (acc : card_map) =
  match lines with
  | [] -> acc
  | h :: rest ->
    (match String.split_on_char ':' h with
     | [ card; card_results ] ->
       process_lines_acc
         rest
         max_row
         (process_card_results
            card_results
            (derive_card_number card)
            max_row
            acc)
     | _ -> failwith "invalid line")
;;

let process_lines lines =
  process_lines_acc lines (List.length lines) CardMap.empty
;;

let part1 =
  let card_map = "input/4.txt" |> File.read_lines |> process_lines in
  CardMap.fold (fun _ card acc -> acc + card.value) card_map 0
;;

let part2 =
  let card_map = "input/4.txt" |> File.read_lines |> process_lines in
  CardMap.fold (fun _ card acc -> acc + card.count) card_map 0
;;
