let get_ints line =
  match String.split_on_char ':' line with
  | [ _; nums ] ->
    nums |> String.trim |> Util.split_on_whitespace |> List.map int_of_string
  | _ -> failwith "uh uh"
;;

let get_combined_ints line =
  match String.split_on_char ':' line with
  | [ _; nums ] ->
    [ nums
      |> String.trim
      |> Util.split_on_whitespace
      |> List.fold_left ( ^ ) ""
      |> int_of_string
    ]
  | _ -> failwith "uh uh"
;;

let get_num_victories time distance =
  let rec get_num_victories_rec time distance hold acc =
    match hold with
    | 0 -> acc
    | ms ->
      (match (time - ms) * ms with
       | x when x > distance ->
         get_num_victories_rec time distance (ms - 1) (acc + 1)
       | _ -> get_num_victories_rec time distance (ms - 1) acc)
  in
  get_num_victories_rec time distance time 0
;;

let process_lines process_line lines =
  match lines with
  | [ first; second ] -> process_line first, process_line second
  | _ -> failwith "nope"
;;

let part1 =
  let times, distances =
    "input/6.txt" |> File.read_lines |> process_lines get_ints
  in
  List.map2 get_num_victories times distances |> List.fold_left ( * ) 1
;;

let part2 =
  let times, distances =
    "input/6.txt" |> File.read_lines |> process_lines get_combined_ints
  in
  List.map2 get_num_victories times distances |> List.fold_left ( * ) 1
;;
