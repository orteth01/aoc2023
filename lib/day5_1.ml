let is_map_string s =
  if String.length s > 4 then Str.last_chars s 4 = "map:" else false
;;

let is_num_string s = List.length (Util.split_on_whitespace s) = 3

let derive_seeds first_line =
  match String.split_on_char ':' first_line with
  | [ _; seeds_data ] ->
    seeds_data
    |> String.trim
    |> Util.split_on_whitespace
    |> List.map int_of_string
  | _ -> failwith "invalid first line"
;;

let process_num_string s (seeds : int list) =
  let nums = s |> Util.split_on_whitespace |> List.map int_of_string in
  match nums with
  | [ destination; source; length ] ->
    List.map
      (fun seed ->
        if seed >= source && seed < source + length
        then (destination + (seed - source)) * -1
        else seed)
      seeds
  | _ -> failwith "not possible"
;;

let reset_num_strings seeds =
  List.map (fun seed -> if seed < 0 then seed * -1 else seed) seeds
;;

let rec derive_next_seeds lines (seeds : int list) =
  match lines with
  | [] -> reset_num_strings seeds
  | head :: rest ->
    let seeds =
      match head with
      | _ when is_map_string head -> reset_num_strings seeds
      | _ when is_num_string head -> process_num_string head seeds
      | _ -> seeds
    in
    derive_next_seeds rest seeds
;;

let process_maps lines seeds = derive_next_seeds lines seeds

let part1 =
  let lines = "input/5.txt" |> File.read_lines in
  let locations =
    match lines with
    | head :: rest ->
      let seeds = derive_seeds head in
      process_maps rest seeds
    | _ -> failwith "lol why no list?"
  in
  List.fold_left min max_int locations
;;
