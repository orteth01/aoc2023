(* string *)
let trim_and_split_on_char c s = s |> String.trim |> String.split_on_char c
let explode s = s |> String.to_seq |> List.of_seq
let split_on_whitespace str = Str.split (Str.regexp "[ ]+") str

let%test "split_on_whitespace" =
  split_on_whitespace "hi   hello cool       beans"
  = [ "hi"; "hello"; "cool"; "beans" ]
;;

(* char *)
let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

(* comparison *)
let max_with_string i s = s |> int_of_string |> max i
let max_with_strings s1 s2 = max (int_of_string s1) (int_of_string s2)

(* debugging *)
let print_int_list lst () =
  List.iter (fun x -> Printf.printf "%d, " x) lst;
  Printf.printf "\n"
;;

let print_int_tuple_list lst () =
  List.iter (fun x -> Printf.printf "(%d,%d), " (fst x) (snd x)) lst;
  Printf.printf "\n"
;;
