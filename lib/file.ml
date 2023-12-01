let read_lines file =
  (* let current_dir = Unix.getcwd () in *)
  let input_channel = open_in ("/Users/teddyortega/dev/aoc2023/" ^ file) in
  let rec read_lines_aux acc =
    try
      let line = input_line input_channel in
      read_lines_aux (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_lines_aux []
;;
