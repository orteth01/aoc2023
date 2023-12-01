open Str

let value_hash = Hashtbl.create 123456;;

Hashtbl.add value_hash "1" "1";;
Hashtbl.add value_hash "one" "1";;
Hashtbl.add value_hash "2" "2";;
Hashtbl.add value_hash "two" "2";;
Hashtbl.add value_hash "3" "3";;
Hashtbl.add value_hash "three" "3";;
Hashtbl.add value_hash "4" "4";;
Hashtbl.add value_hash "four" "4";;
Hashtbl.add value_hash "5" "5";;
Hashtbl.add value_hash "five" "5";;
Hashtbl.add value_hash "6" "6";;
Hashtbl.add value_hash "six" "6";;
Hashtbl.add value_hash "7" "7";;
Hashtbl.add value_hash "seven" "7";;
Hashtbl.add value_hash "8" "8";;
Hashtbl.add value_hash "eight" "8";;
Hashtbl.add value_hash "9" "9";;
Hashtbl.add value_hash "nine" "9"

let find_regex_forward line regex =
  ignore (Str.search_forward regex line 0);
  Str.matched_string line
;;

let find_regex_backward line regex =
  let len = String.length line in
  ignore (Str.search_backward regex line len);
  Str.matched_string line
;;

let process_line line regex =
  int_of_string
    (Hashtbl.find value_hash (find_regex_forward line regex)
     ^ Hashtbl.find value_hash (find_regex_backward line regex))
;;

let part1 =
  let lines = File.read_lines "input/1.txt" in
  let search_regex = regexp "[1-9]" in
  List.fold_left
    ( + )
    0
    (List.map (fun line -> process_line line search_regex) lines)
;;

let%test _ = part1 = 54634

let part2 =
  let lines = File.read_lines "input/1.txt" in
  let search_regex =
    regexp
      "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[1-9]"
  in
  List.fold_left
    ( + )
    0
    (List.map (fun line -> process_line line search_regex) lines)
;;

let%test _ = part2 = 53855
