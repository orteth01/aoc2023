open List
open Util

type pipe =
  | NS
  | EW
  | NE
  | NW
  | SW
  | SE
  | Ground
  | Start

type node_status =
  | NotVisited
  | Visited

let map_char_to_pipe char =
  match char with
  | '|' -> NS
  | '-' -> EW
  | 'L' -> NE
  | 'J' -> NW
  | '7' -> SW
  | 'F' -> SE
  | '.' -> Ground
  | 'S' -> Start
  | _ -> failwith "invalid character"
;;

let find_start matrix max_row max_col =
  let rec search_row' row col =
    if col >= max_col
    then None
    else (
      match matrix.(row).(col) with
      | Start -> Some (row, col)
      | _ -> search_row' row (col + 1))
  in
  let rec search_col' row =
    if row >= max_row
    then None
    else (
      match search_row' row 0 with
      | Some position -> Some position
      | None -> search_col' (row + 1))
  in
  search_col' 0
;;

let is_valid_move matrix visited_matrix r c =
  r >= 0
  && r < Array.length matrix
  && c >= 0
  && c < Array.length matrix.(0)
  && visited_matrix.(r).(c) = NotVisited
;;

let is_valid_pipe_move matrix from_r from_c to_r to_c =
  let to_pipe = matrix.(to_r).(to_c) in
  let from_pipe = matrix.(from_r).(from_c) in
  if from_r < to_r
  then
    (* going south *)
    (to_pipe = NE || to_pipe = NS || to_pipe = NW)
    && (from_pipe = Start || from_pipe = SE || from_pipe = SW || from_pipe = NS)
  else if from_r > to_r
  then
    (* going north *)
    (to_pipe = SE || to_pipe = SW || to_pipe = NS)
    && (from_pipe = Start || from_pipe = NS || from_pipe = NW || from_pipe = NE)
  else if from_c < to_c
  then
    (* going east *)
    (to_pipe = NW || to_pipe = SW || to_pipe = EW)
    && (from_pipe = Start || from_pipe = EW || from_pipe = NE || from_pipe = SE)
  else if from_c > to_c
  then
    (* going west *)
    (to_pipe = NE || to_pipe = SE || to_pipe = EW)
    && (from_pipe = Start || from_pipe = EW || from_pipe = SW || from_pipe = NW)
  else false
;;

let rec traverse matrix visited_matrix row col boundary_coords =
  visited_matrix.(row).(col) <- Visited;
  let neighbors = [ row - 1, col; row + 1, col; row, col - 1; row, col + 1 ] in
  match
    List.find_opt
      (fun (next_row, next_col) ->
        is_valid_move matrix visited_matrix next_row next_col
        && is_valid_pipe_move matrix row col next_row next_col)
      neighbors
  with
  | Some v ->
    let next_row = fst v in
    let next_col = snd v in
    traverse
      matrix
      visited_matrix
      next_row
      next_col
      (List.append boundary_coords [ next_row, next_col ])
  | None -> boundary_coords
;;

let get_boundary_coords =
  let lines = "input/10.txt" |> File.read_lines in
  let matrix =
    lines
    |> map (fun line ->
      line |> explode |> List.map map_char_to_pipe |> Array.of_list)
    |> Array.of_list
  in
  let max_row = Array.length matrix in
  let max_col = Array.length matrix.(0) in
  let visited_matrix = Array.make_matrix max_row max_col NotVisited in
  let start_row, start_col =
    match find_start matrix max_row max_col with
    | Some v -> v
    | None -> failwith "no start char"
  in
  let boundary_coords = traverse matrix visited_matrix start_row start_col [] in
  (* put start coords at beginning and end of coord list *)
  let with_start = (start_row, start_col) :: boundary_coords in
  List.append with_start [ start_row, start_col ]
;;

let part1 =
  let boundary_coords = get_boundary_coords in
  List.length boundary_coords / 2
;;

(* let%test _ = Printf.printf "part 1: %d\n" part1; part1 = 7030 ;; *)

(* https://en.wikipedia.org/wiki/Shoelace_formula *)
let rec shoe_lace' list acc =
  match list with
  | [] | [ _ ] -> abs acc / 2
  | first :: second :: rest ->
    let new_acc = acc + ((fst first * snd second) - (snd first * fst second)) in
    shoe_lace' (second :: rest) new_acc
;;

let part2 =
  let boundary_coords = get_boundary_coords in
  let area = shoe_lace' boundary_coords 0 in
  area - (List.length boundary_coords / 2) + 1
;;

(* let%test _ = Printf.printf "part 2: %d\n" part2; part2 = 285 ;; *)
