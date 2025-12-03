type range = { start : int; stop : int }

let int_list_to_string list =
  list |> List.map (fun item -> Int.to_string item) |> String.concat ", "

let sum numbers = List.fold_left (fun acc num -> acc + num) 0 numbers

let string_repeat pattern repeat =
  List.init repeat (fun _ -> pattern) |> String.concat ""

let range_to_string range =
  "Range [" ^ Int.to_string range.start ^ "," ^ Int.to_string range.stop ^ "]"

let is_within_range range number =
  (* let () = *)
  (*   print_endline *)
  (*     ("Checking if " ^ Int.to_string number ^ " belongs to " *)
  (*    ^ range_to_string range) *)
  (* in *)
  number >= range.start && number <= range.stop

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
