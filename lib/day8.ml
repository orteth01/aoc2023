open Str

module NodeMap = Map.Make (struct
    type t = string

    let compare = compare
  end)

type node_map = string * string NodeMap.t

type directions_accum =
  { current_check : string
  ; num_steps : int
  }

let lowest_common_multiple_of_list list =
  match list with
  | [] -> 0
  | head :: rest -> List.fold_left Util.lowest_common_multiple head rest
;;

let add_map_entry line starters map =
  if string_match
       (regexp
          "\\([A-Z][A-Z][A-Z]\\) = (\\([A-Z][A-Z][A-Z]\\), \
           \\([A-Z][A-Z][A-Z]\\))")
       line
       0
  then (
    let key = matched_group 1 line in
    let l_dir = matched_group 2 line in
    let r_dir = matched_group 3 line in
    let new_map = NodeMap.add key (l_dir, r_dir) map in
    let new_starters =
      if Util.ends_with_char key 'A'
      then List.append starters [ key ]
      else starters
    in
    new_map, new_starters)
  else map, starters
;;

let build_map lines =
  let rec build_map_rec lines map starters =
    match lines with
    | [] -> map, starters
    | head :: rest ->
      let new_map, new_starters = add_map_entry head starters map in
      build_map_rec rest new_map new_starters
  in
  build_map_rec lines NodeMap.empty []
;;

let char_dir char =
  match char with
  | 'L' -> fst
  | 'R' -> snd
  | _ -> failwith "invalid direction"
;;

let follow_directions full_directions is_found map starter =
  let rec follow_dirs_rec direcs acc =
    match direcs, acc.current_check with
    | _, c when is_found c -> acc.num_steps
    | [], _ -> follow_dirs_rec full_directions acc
    | head :: rest, _ ->
      follow_dirs_rec
        rest
        { current_check = (char_dir head) (NodeMap.find acc.current_check map)
        ; num_steps = acc.num_steps + 1
        }
  in
  follow_dirs_rec full_directions { current_check = starter; num_steps = 0 }
;;

let part1 =
  let lines = "input/8.txt" |> File.read_lines in
  match lines with
  | head :: _ :: rest ->
    let full_directions = Util.explode head in
    let map, _ = build_map rest in
    let is_found key = key = "ZZZ" in
    follow_directions full_directions is_found map "AAA"
  | _ -> failwith "uh oh"
;;

let%test "part1" =
  Printf.printf "%i\n" part1;
  part1 = 18113
;;

let part2 =
  let lines = "input/8.txt" |> File.read_lines in
  match lines with
  | head :: _ :: rest ->
    let full_directions = Util.explode head in
    let map, starters = build_map rest in
    let is_found key = Util.ends_with_char key 'Z' in
    List.map
      (fun starter -> follow_directions full_directions is_found map starter)
      starters
    |> lowest_common_multiple_of_list
  | _ -> failwith "uh oh"
;;

let%test "part2" =
  Printf.printf "%i\n" part2;
  part2 = 12315788159977
;;
