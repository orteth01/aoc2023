open List

let conditional_reverse condition list = if condition then rev list else list

let rec get_difs' list =
  match list with
  | [] | [ _ ] -> []
  | first :: second :: rest -> (second - first) :: get_difs' (second :: rest)
;;

let get_dif_stack reverse line =
  let rec get_dif_stack' acc ints =
    if for_all (fun i -> i = 0) ints
    then acc
    else (
      let difs = get_difs' ints in
      let to_add = difs |> conditional_reverse reverse |> hd in
      get_dif_stack' (to_add :: acc) difs)
  in
  let ints = line |> Util.split_on_whitespace |> map int_of_string in
  get_dif_stack' [ ints |> conditional_reverse reverse |> hd ] ints
;;

let part1 =
  "input/9.txt"
  |> File.read_lines
  |> map (fun line -> get_dif_stack true line |> fold_left ( + ) 0)
  |> fold_left ( + ) 0
;;

let%test "part1" =
  Printf.printf "%i\n" part1;
  part1 = 1930746032
;;

let part2 =
  "input/9.txt"
  |> File.read_lines
  |> map (fun line ->
    get_dif_stack false line |> fold_left (fun acc i -> i - acc) 0)
  |> fold_left ( + ) 0
;;

let%test "part2" =
  Printf.printf "%i\n" part2;
  part2 = 1154
;;
