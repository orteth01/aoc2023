module CoordMap = Map.Make (struct
    type t = int * int

    let compare = compare
  end)

module CoordSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

type symbol_tracker =
  { char : char
  ; neighbor_nums : int list
  }

type symbol_tracker_by_coord = symbol_tracker CoordMap.t

type number_traversal_state =
  { number_string : string
  ; neighbor_coords : CoordSet.t
  ; coord_map : symbol_tracker_by_coord
  }

let add_symbol_to_map key char map =
  CoordMap.add key { char; neighbor_nums = [] } map
;;

let neighbor_coords row_num col_num =
  CoordSet.empty
  |> CoordSet.add (row_num - 1, col_num + 1)
  |> CoordSet.add (row_num - 1, col_num)
  |> CoordSet.add (row_num - 1, col_num - 1)
  |> CoordSet.add (row_num, col_num - 1)
  |> CoordSet.add (row_num, col_num + 1)
  |> CoordSet.add (row_num + 1, col_num + 1)
  |> CoordSet.add (row_num + 1, col_num)
  |> CoordSet.add (row_num + 1, col_num - 1)
;;

let create_symbol_coord_map chars_matrix =
  let rec traverse_cols_for_symbol line coord_map row_num col_num =
    match line with
    | [] -> coord_map
    | char :: rest ->
      let map =
        if (not (Util.is_digit char)) && not (char = '.')
        then add_symbol_to_map (row_num, col_num) char coord_map
        else coord_map
      in
      traverse_cols_for_symbol rest map row_num (col_num + 1)
  in
  let rec traverse_rows_for_symbol arr_2d coord_map row_num =
    match arr_2d with
    | [] -> coord_map
    | row :: rest ->
      let map = traverse_cols_for_symbol row coord_map row_num 0 in
      traverse_rows_for_symbol rest map (row_num + 1)
  in
  traverse_rows_for_symbol chars_matrix CoordMap.empty 0
;;

let find_first_common_key map set =
  let find key acc = if CoordMap.mem key map then Some key else acc in
  CoordSet.fold find set None
;;

let add_number_to_symbol_map map number neighbor_coords =
  match find_first_common_key map neighbor_coords with
  | None -> map
  | Some neighbor_symbol_coords ->
    let current_map_value =
      match CoordMap.find_opt neighbor_symbol_coords map with
      | Some value -> value
      | None -> failwith "not possible"
    in
    CoordMap.add
      neighbor_symbol_coords
      { current_map_value with
        neighbor_nums = number :: current_map_value.neighbor_nums
      }
      map
;;

let traverse_nums chars_matrix coord_map =
  let rec traverse_cols_for_numbers line state row_num col_num =
    let reset_state =
      { state with number_string = ""; neighbor_coords = CoordSet.empty }
    in
    match line with
    | [] ->
      if String.length state.number_string = 0
      then reset_state
      else (
        (* if we hit the end of the line but we've got number string and
           neighbors in state, we've reached the end of a number. if it has a
           symbol neighbor, add it to that symbol's neighbor_nums *)
        let new_map_value =
          add_number_to_symbol_map
            state.coord_map
            (int_of_string state.number_string)
            state.neighbor_coords
        in
        { reset_state with coord_map = new_map_value })
    | char :: rest ->
      let state =
        if Util.is_digit char
        then (
          (* if we encounter a digit, add it to the number string in state and
             note its neighbors *)
          let new_number_string = state.number_string ^ String.make 1 char in
          let new_neighbor_coords =
            CoordSet.union
              state.neighbor_coords
              (neighbor_coords row_num col_num)
          in
          { state with
            number_string = new_number_string
          ; neighbor_coords = new_neighbor_coords
          })
        else if String.length state.number_string = 0
        then reset_state
        else (
          (* if not a char but we've got number string and neighbors in state,
             we've reached the end of a number. if it has a symbol neighbor, add
             it to that symbol's neighbor_nums *)
          let new_map_value =
            add_number_to_symbol_map
              state.coord_map
              (int_of_string state.number_string)
              state.neighbor_coords
          in
          { number_string = ""
          ; neighbor_coords = CoordSet.empty
          ; coord_map = new_map_value
          })
      in
      traverse_cols_for_numbers rest state row_num (col_num + 1)
  in
  let rec traverse_rows_for_numbers arr_2d state row_num =
    match arr_2d with
    | [] -> state
    | row :: rest ->
      let new_state = traverse_cols_for_numbers row state row_num 0 in
      traverse_rows_for_numbers rest new_state (row_num + 1)
  in
  traverse_rows_for_numbers
    chars_matrix
    { number_string = ""; neighbor_coords = CoordSet.empty; coord_map }
    0
;;

let part1 =
  let chars_matrix =
    "input/3.txt"
    |> File.read_lines
    |> List.map (fun line -> line |> Util.explode)
  in
  let symbol_coord_map = create_symbol_coord_map chars_matrix in
  let num_traversal_result = traverse_nums chars_matrix symbol_coord_map in
  CoordMap.fold
    (fun _ value acc ->
      value.neighbor_nums |> List.fold_left ( + ) 0 |> ( + ) acc)
    num_traversal_result.coord_map
    0
;;

let part2 =
  let chars_matrix =
    "input/3.txt"
    |> File.read_lines
    |> List.map (fun line -> line |> Util.explode)
  in
  let symbol_coord_map = create_symbol_coord_map chars_matrix in
  let num_traversal_result = traverse_nums chars_matrix symbol_coord_map in
  CoordMap.fold
    (fun _ value acc ->
      if value.char = '*' && List.length value.neighbor_nums = 2
      then value.neighbor_nums |> List.fold_left ( * ) 1 |> ( + ) acc
      else acc)
    num_traversal_result.coord_map
    0
;;
