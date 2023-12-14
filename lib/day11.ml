open List
open Util
open Printf

type universe_space =
  | Galaxy
  | Space

let map_char char =
  match char with
  | '#' -> Galaxy
  | '.' -> Space
  | _ -> failwith "invalid character"
;;

let is_num_between x y num = (x < num && y > num) || (x > num && y < num)

let rec transpose_matrix matrix =
  match matrix with
  | [] | [] :: _ -> []
  | _ -> map hd matrix :: transpose_matrix (map tl matrix)
;;

let get_empty_rows matrix =
  matrix
  |> fold_left
       (fun (row_num, empty_rows) line ->
         if for_all (fun char -> char = Space) line
         then row_num + 1, row_num :: empty_rows
         else row_num + 1, empty_rows)
       (0, [])
;;

let get_galaxy_coords matrix =
  let rec get_galaxy_coords' m row col col_acc =
    match m with
    | [] -> col_acc
    | row_values :: remaining_rows ->
      let rec find_in_row col row_values2 row_acc =
        match row_values2 with
        | [] -> get_galaxy_coords' remaining_rows (row + 1) 0 row_acc
        | Galaxy :: rest -> find_in_row (col + 1) rest ((row, col) :: row_acc)
        | _ :: rest -> find_in_row (col + 1) rest row_acc
      in
      find_in_row col row_values col_acc
  in
  get_galaxy_coords' matrix 0 0 []
;;

let rec get_sum_of_paths' empty_rows empty_cols expansion_val acc coord_list =
  match coord_list with
  | [] -> acc
  | [ _ ] -> acc
  | (galaxy_x, galaxy_y) :: rest ->
    get_sum_of_paths'
      empty_rows
      empty_cols
      expansion_val
      (rest
       |> fold_left
            (* compare each remaining galaxy with the current one *)
              (fun acc (galaxy_2_x, galaxy_2_y) ->
              let distance_without_expansion =
                abs (galaxy_y - galaxy_2_y) + abs (galaxy_x - galaxy_2_x)
              in
              let empty_rows_between =
                empty_rows
                |> filter (is_num_between galaxy_x galaxy_2_x)
                |> length
              in
              let empty_cols_between =
                empty_cols
                |> filter (is_num_between galaxy_y galaxy_2_y)
                |> length
              in
              acc
              + distance_without_expansion
              + (empty_rows_between * (expansion_val - 1))
              + (empty_cols_between * (expansion_val - 1)))
            acc)
      rest
;;

let part1 =
  let matrix =
    File.read_lines "input/11.txt"
    |> map (fun line -> line |> explode |> map map_char)
  in
  let expansion_value = 2 in
  let empty_rows = snd (matrix |> get_empty_rows) in
  let empty_cols = snd (matrix |> transpose_matrix |> get_empty_rows) in
  matrix
  |> get_galaxy_coords
  |> get_sum_of_paths' empty_rows empty_cols expansion_value 0
;;

let%test _ =
  printf "part 1: %d\n" part1;
  part1 = 9550717
;;

let part2 =
  let matrix =
    File.read_lines "input/11.txt"
    |> map (fun line -> line |> explode |> map map_char)
  in
  let expansion_value = 1000000 in
  let empty_rows = snd (matrix |> get_empty_rows) in
  let empty_cols = snd (matrix |> transpose_matrix |> get_empty_rows) in
  matrix
  |> get_galaxy_coords
  |> get_sum_of_paths' empty_rows empty_cols expansion_value 0
;;

let%test _ =
  printf "part 2: %d\n" part2;
  part2 = 648458253817
;;
