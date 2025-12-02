open Helpers

let apply_instruction ((direction, distance) : instruction) (base_number : int)
    =
  let raw_number =
    match direction with
    | Left -> base_number - distance
    | Right -> base_number + distance
  in
  raw_number mod 100

let execute_first_part =
  let code, _ =
    read_input ()
    |> List.fold_left
         (fun (code, base_number) instruction_as_text ->
           let mb_instruction = instruction_as_text |> parse_instruction in
           match mb_instruction with
           | Some (direction, distance) ->
               let new_base_number =
                 apply_instruction (direction, distance) base_number
               in
               ( (if new_base_number == 0 then code + 1 else code),
                 new_base_number )
           | None -> (code, base_number))
         (0, start_number)
  in
  print_endline ("Result is " ^ Int.to_string code)
