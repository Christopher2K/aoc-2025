[@@@warning "-32"]

type range = { start : int; stop : int }

let input file_path () =
  In_channel.open_text file_path
  |> In_channel.input_line
  |> Option.map (fun line -> String.split_on_char ',' line)
  |> Option.map (fun range_list ->
         range_list
         |> List.fold_left
              (fun parsed_ranges range_str ->
                let range_tuple = String.split_on_char '-' range_str in
                match range_tuple with
                | [ x; y ] -> (
                    match (int_of_string_opt x, int_of_string_opt y) with
                    | Some x_int, Some y_int ->
                        List.append parsed_ranges
                          [ { start = x_int; stop = y_int } ]
                    | _ -> parsed_ranges)
                | _ -> parsed_ranges)
              [])

let get_input = input "./bin/input.txt"
let get_input_test = input "./bin/input-test.txt"

let int_list_to_string list =
  list |> List.map (fun item -> Int.to_string item) |> String.concat ", "

let range_to_string range =
  "Range [" ^ Int.to_string range.start ^ "," ^ Int.to_string range.stop ^ "]"

let is_within_range range number =
  let () =
    print_endline
      ("Checking if " ^ Int.to_string number ^ " belongs to "
     ^ range_to_string range)
  in
  number >= range.start && number <= range.stop

let get_range_invalid_ids range =
  let range_start_length = range.start |> Int.to_string |> String.length in
  let range_stop_length = range.stop |> Int.to_string |> String.length in
  if range_start_length mod 2 == 0 then
    let () = print_endline ("Testing " ^ range_to_string range) in
    let start_str = Int.to_string range.start in
    let initial_prefix_str =
      String.sub start_str 0 (String.length start_str / 2)
    in

    let rec loop number_list prefix =
      let prefix_str = Int.to_string prefix in
      let nb_str = prefix_str ^ prefix_str in
      let () = print_endline ("\tTesting -> " ^ nb_str) in

      match int_of_string_opt nb_str with
      | Some nb when is_within_range range nb ->
          loop (List.append number_list [ nb ]) (prefix + 1)
      | Some nb when nb <= range.stop -> loop number_list (prefix + 1)
      | _ -> number_list
    in
    match int_of_string_opt initial_prefix_str with
    | Some nb -> loop [] nb
    | None -> []
  else if range_stop_length mod 2 == 0 then
    let () = print_endline ("Testing " ^ range_to_string range) in
    let stop_str = Int.to_string range.stop in
    let initial_prefix_str =
      String.sub stop_str 0 (String.length stop_str / 2)
    in

    let rec loop number_list prefix =
      let prefix_str = Int.to_string prefix in
      let nb_str = prefix_str ^ prefix_str in
      let () = print_endline ("\tTesting -> " ^ nb_str) in

      match int_of_string_opt nb_str with
      | Some nb when is_within_range range nb ->
          loop (List.append number_list [ nb ]) (prefix - 1)
      | Some nb when nb >= range.start -> loop number_list (prefix - 1)
      | _ -> number_list
    in
    match int_of_string_opt initial_prefix_str with
    | Some nb -> loop [] nb
    | None -> []
  else []

let () =
  get_input ()
  |> Option.iter (fun range_list ->
         let invalid_ids =
           range_list |> List.concat_map get_range_invalid_ids
         in
         let result =
           List.fold_left (fun acc num -> acc + num) 0 invalid_ids
           |> Int.to_string
         in
         print_endline ("Result: " ^ result))
