open Helpers

let apply_instruction ((direction, distance) : instruction) (base_number : int)
    =
  let new_score =
    match direction with
    | Left -> base_number - distance
    | Right -> base_number + distance
  in
  let () = print_endline ("New computed score: " ^ Int.to_string new_score) in
  let result =
    match new_score with
    | x when x > 0 ->
        let times_zero_was_displayed = new_score / 100 in
        let next_base_number = new_score mod 100 in
        (times_zero_was_displayed, next_base_number)
    | x when x < 0 ->
        let should_add_one = distance > base_number && base_number <> 0 in
        let bonus = if should_add_one then 1 else 0 in
        let abs_new_score = Int.abs new_score in
        let times_zero_was_displayed = (abs_new_score / 100) + bonus in
        let next_base_number = (100 - (abs_new_score mod 100)) mod 100 in
        (times_zero_was_displayed, next_base_number)
    | _ -> (1, 0)
    (* when x is zero *)
  in
  let () =
    print_endline ("Time 0 was displayed -> " ^ Int.to_string (fst result))
  in
  let () =
    print_endline ("Next base number -> " ^ Int.to_string (snd result))
  in
  let () = print_endline "********" in
  result

let execute_last_part =
  let code, _ =
    read_input ()
    |> List.fold_left
         (fun (code, base_number) instruction_as_text ->
           let () = print_endline ("Working With :" ^ instruction_as_text) in
           let mb_instruction = instruction_as_text |> parse_instruction in
           match mb_instruction with
           | Some instruction ->
               let instruction_code, new_base_number =
                 apply_instruction instruction base_number
               in
               (code + instruction_code, new_base_number)
           | None -> (code, base_number))
         (0, start_number)
  in
  print_endline ("Result is " ^ Int.to_string code)
