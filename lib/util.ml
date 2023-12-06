let trim_and_split_on_char c s = s |> String.trim |> String.split_on_char c
let explode s = s |> String.to_seq |> List.of_seq
let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
let max_with_string i s = s |> int_of_string |> max i
let max_with_strings s1 s2 = max (int_of_string s1) (int_of_string s2)
