open Helpers

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

let execute_part_one =
  get_input ()
  |> Option.iter (fun range_list ->
         let result =
           range_list
           |> List.concat_map get_range_invalid_ids
           |> sum |> Int.to_string
         in
         print_endline ("Result: " ^ result))
