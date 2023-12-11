open Str

let str_to_val num_str =
  match num_str with
  | "1" -> "1"
  | "one" -> "1"
  | "2" -> "2"
  | "two" -> "2"
  | "3" -> "3"
  | "three" -> "3"
  | "4" -> "4"
  | "four" -> "4"
  | "5" -> "5"
  | "five" -> "5"
  | "6" -> "6"
  | "six" -> "6"
  | "7" -> "7"
  | "seven" -> "7"
  | "8" -> "8"
  | "eight" -> "8"
  | "9" -> "9"
  | "nine" -> "9"
  | _ -> failwith "invalid number string"
;;

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
    (str_to_val (find_regex_forward line regex)
     ^ str_to_val (find_regex_backward line regex))
;;

let part1 =
  File.read_lines "input/1.txt"
  |> List.map (fun line -> process_line line (regexp "[1-9]"))
  |> Util.sum_int_list
;;

let part2 =
  File.read_lines "input/1.txt"
  |> List.map (fun line ->
    process_line
      line
      (regexp
         "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[1-9]"))
  |> Util.sum_int_list
;;
