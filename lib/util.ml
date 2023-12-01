let trim_and_split_on_char char string =
  string |> String.trim |> String.split_on_char char
;;

let explode string = string |> String.to_seq |> List.of_seq
let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
