open Helpers

(**
   Find the boundaries of invalid sequences with a specific number of digit

  (* let execute = *)
  (*   find_invalid_ids_bounds_for_digit 6 *)
  (*   |> Option.iter (fun (s, e) -> *)
  (*          print_endline *)
  (*            ("Boudaries : " ^ Int.to_string s ^ ", " ^ Int.to_string e)) *)
*)
let find_invalid_ids_bounds_for_digit (digit : int) =
  match digit with
  | 1 -> None
  | 2 -> Some (11, 99)
  | x ->
      if x mod 2 == 1 then
        Some
          ( string_repeat "1" x |> int_of_string,
            string_repeat "9" x |> int_of_string )
      else
        Some
          ( string_repeat "10" (x / 2) |> int_of_string,
            string_repeat "9" x |> int_of_string )

(**
   For a specific number, find how long can be a repeated sequence

  (* let execute = *)
  (*   find_all_possible_invalid_ids_length 203243114324 *)
  (*   |> int_list_to_string |> print_endline *)
*)
let find_all_possible_invalid_sequence_length number =
  let length = number |> Int.to_string |> String.length in
  if length == 1 then []
  else
    let rec loop result current_number =
      if current_number == 1 then result
      else
        match length mod current_number with
        | 0 -> loop (List.append result [ current_number ]) (current_number - 1)
        | _ -> loop result (current_number - 1)
    in
    List.append (loop [] (length / 2)) [ 1 ]

(**
   Find all invalid id for numbers with a specific number of digits

(* let execute = *)
(*   find_all_invalid_ids_for_a_digit_range 12 *)
(*   |> int_list_to_string |> print_endline *)
*)
let find_all_invalid_ids_for_a_digit_range digit =
  match digit with
  | 1 -> []
  | x when x == 2 ->
      List.init 9 (fun y ->
          string_repeat (Int.to_string (y + 1)) digit |> int_of_string)
  | _ ->
      let invalid_sequence_lengths =
        find_all_possible_invalid_sequence_length
          ("1" ^ string_repeat "0" (digit - 1) |> int_of_string)
      in
      let result =
        invalid_sequence_lengths
        |> List.concat_map (fun current_length ->
               let lower_bound, upper_bound =
                 ( "1" ^ string_repeat "0" (current_length - 1) |> int_of_string,
                   "9" ^ string_repeat "9" (current_length - 1) |> int_of_string
                 )
               in
               let range_length = upper_bound + 1 - lower_bound in
               List.init range_length (fun x -> lower_bound + x)
               |> List.map (fun sequence ->
                      let sequence_str = Int.to_string sequence in
                      string_repeat sequence_str (digit / current_length)
                      |> int_of_string))
      in
      result |> List.sort_uniq compare

(**
   For a specific range, find all how many digits a number can have

(* let execute = *)
(*   find_digit_spans_in_range { start = 23; stop = 3345 } *)
(*   |> int_list_to_string |> print_endline *)
*)
let find_digit_spans_in_range range =
  let { start; stop } = range in
  let digit_start = start |> Int.to_string |> String.length in
  let digit_stop = stop |> Int.to_string |> String.length in
  List.init (digit_stop + 1 - digit_start) (fun x -> digit_start + x)

let find_all_invalid_ids_in_range range =
  range |> find_digit_spans_in_range
  |> List.concat_map (fun digit -> find_all_invalid_ids_for_a_digit_range digit)
  |> List.filter (fun invalid_id -> is_within_range range invalid_id)

let execute =
  get_input ()
  |> Option.iter (fun range_list ->
         let result =
           range_list
           |> List.concat_map find_all_invalid_ids_in_range
           |> List.sort_uniq compare |> sum
         in

         print_endline ("Result: " ^ Int.to_string result))
