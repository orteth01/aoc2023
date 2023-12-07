let is_map_string s =
  if String.length s > 4 then Str.last_chars s 4 = "map:" else false
;;

let is_num_string s = s |> Util.split_on_whitespace |> List.length = 3

let derive_initial_seed_pairs first_line =
  let rec derive_initial_seed_pairs_acc acc ints =
    match ints with
    | [] -> acc
    | first :: second :: rest ->
      derive_initial_seed_pairs_acc (List.append acc [ first, second ]) rest
    | _ -> failwith "not possible"
  in
  match String.split_on_char ':' first_line with
  | [ _; seeds_data ] ->
    seeds_data
    |> String.trim
    |> Util.split_on_whitespace
    |> List.map int_of_string
    |> derive_initial_seed_pairs_acc []
  | _ -> failwith "invalid first line"
;;

let reset_seed_pairs seed_pairs =
  List.map
    (fun (value, range) ->
      if value < 0 then value * -1, range else value, range)
    seed_pairs
;;

let process_seed_pairs map_string seed_pairs =
  match map_string |> Util.split_on_whitespace |> List.map int_of_string with
  | [ destination; source; source_range ] ->
    List.fold_left
      (fun acc (min_seed, seed_range) ->
        List.append
          acc
          (if min_seed < 0
           then [ min_seed, seed_range ]
           else (
             (* --------------------- *)
             let change = destination - source in
             let max_source = source + source_range - 1 in
             let max_seed = min_seed + seed_range - 1 in
             let start_intersect = max source min_seed in
             let end_intersect = min max_source max_seed in
             let length_intersect = max 0 (end_intersect - start_intersect) in
             (* --------------------- *)
             if length_intersect = 0 (* no intersection, just pass through*)
             then [ min_seed, seed_range ]
             else
               (* there is intersection *)
               (* add the intersected range (with the change offset) *)
               [ (start_intersect + change) * -1, length_intersect ]
               @ (if min_seed < start_intersect
                     (* if there was excess in the front, pass it along*)
                  then [ min_seed, start_intersect - min_seed ]
                  else [])
               @
               if max_seed > end_intersect
                  (* if there was excess in the back, pass it along*)
               then [ end_intersect, max_seed - end_intersect ]
               else [])))
      []
      seed_pairs
  | _ -> failwith "not possible"
;;

let rec calculate_next_map_result lines seed_pairs =
  match lines with
  | [] -> reset_seed_pairs seed_pairs
  | head :: rest ->
    let new_seed_pairs =
      match head with
      | _ when is_map_string head -> reset_seed_pairs seed_pairs
      | _ when is_num_string head -> process_seed_pairs head seed_pairs
      | _ -> seed_pairs
    in
    calculate_next_map_result rest new_seed_pairs
;;

let process_maps lines seed_pairs = calculate_next_map_result lines seed_pairs

let part2 =
  let lines = "input/5.txt" |> File.read_lines in
  let location_pairs =
    match lines with
    | head :: rest ->
      let seed_pairs = derive_initial_seed_pairs head in
      process_maps rest seed_pairs
    | _ -> failwith "lol why no list?"
  in
  List.fold_left (fun acc (m, _) -> min acc m) max_int location_pairs
;;

(* let%test "5.2" = Printf.printf "Day5, Part 2: %i \n" part2; part2 = 0 ;; *)
