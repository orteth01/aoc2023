type game_results =
  { green : int
  ; blue : int
  ; red : int
  ; game_number : int
  }

let rec derive_max_draws_acc draws (acc : game_results) : game_results =
  match draws with
  | [] -> acc
  | draw :: rest ->
    (match Util.trim_and_split_on_char ' ' draw with
     | [ number; color ] ->
       let count = int_of_string number in
       (match color with
        | "red" ->
          derive_max_draws_acc rest { acc with red = max acc.red count }
        | "green" ->
          derive_max_draws_acc rest { acc with green = max acc.green count }
        | "blue" ->
          derive_max_draws_acc rest { acc with blue = max acc.blue count }
        | _ -> failwith "invalid color")
     | _ -> failwith "invalid draw")
;;

let derive_game_number game =
  match String.split_on_char ' ' game with
  | [ _; game_id ] -> int_of_string game_id
  | _ -> failwith "invalid game"
;;

let derive_game_results results =
  match String.split_on_char ':' results with
  | [ game; game_result_raw ] ->
    derive_max_draws_acc
      (game_result_raw
       |> Util.trim_and_split_on_char ';'
       |> List.map (Util.trim_and_split_on_char ',')
       |> List.flatten)
      { red = 0; blue = 0; green = 0; game_number = derive_game_number game }
  | _ -> failwith "invalid line"
;;

let part1 =
  let lines = File.read_lines "input/2.txt" in
  lines
  |> List.map (fun line ->
    let game_results = derive_game_results line in
    if game_results.red <= 12
       && game_results.green <= 13
       && game_results.blue <= 14
    then game_results.game_number
    else 0)
  |> List.fold_left ( + ) 0
;;

let%test _ = part1 = 2593

let part2 =
  let lines = File.read_lines "input/2.txt" in
  lines
  |> List.map (fun line ->
    let game_results = derive_game_results line in
    game_results.red * game_results.green * game_results.blue)
  |> List.fold_left ( + ) 0
;;

let%test _ = part2 = 54699
