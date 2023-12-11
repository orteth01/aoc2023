(* string *)
let trim_and_split_on_char c s = s |> String.trim |> String.split_on_char c
let explode s = s |> String.to_seq |> List.of_seq
let%test "explode" = explode "hello" = [ 'h'; 'e'; 'l'; 'l'; 'o' ]
let%test "explode empty" = explode "" = []
let split_on_whitespace str = Str.split (Str.regexp "[ ]+") str

let ends_with_char s c =
  let len = String.length s in
  len > 0 && s.[len - 1] = c
;;

let%test "ends with char = true" = ends_with_char "11A" 'A' = true
let%test "ends with char = false" = ends_with_char "11A" 'B' = false

let%test "split_on_whitespace" =
  split_on_whitespace "hi   hello cool       beans"
  = [ "hi"; "hello"; "cool"; "beans" ]
;;

(* int *)
let greatest_common_devisor a b =
  let rec greatest_common_devisor' x y =
    if y = 0 then x else greatest_common_devisor' y (x mod y)
  in
  greatest_common_devisor' (abs a) (abs b)
;;

let%test "greatest_common_devisor" = greatest_common_devisor 24 16 = 8
let lowest_common_multiple a b = abs (a * b) / greatest_common_devisor a b
let%test "lowest_common_multiple" = lowest_common_multiple 7 11 = 77

(* char *)
let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

(* comparison *)
let max_with_string i s = s |> int_of_string |> max i
let max_with_strings s1 s2 = max (int_of_string s1) (int_of_string s2)
let compare_desc a b = compare b a

(* int list *)
let sum_int_list list = List.fold_left ( + ) 0 list
let%test "sum_int_list" = sum_int_list [ 1; 2; 3; 4; 5 ] = 15

(* debugging *)
let print_str_list lst () =
  List.iter (fun x -> Printf.printf "%s, " x) lst;
  Printf.printf "\n"
;;

let print_int_list lst () =
  List.iter (fun x -> Printf.printf "%d, " x) lst;
  Printf.printf "\n"
;;

let print_int_tuple_list lst () =
  List.iter (fun x -> Printf.printf "(%d,%d), " (fst x) (snd x)) lst;
  Printf.printf "\n"
;;
