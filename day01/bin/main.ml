type instruction_direction = Left | Right
type instruction = instruction_direction * int

let start_number = 50

let read_input () =
  In_channel.open_text "./bin/input.txt" |> In_channel.input_lines

let parse_instruction instruction_as_text =
  let instruction_as_text_length = String.length instruction_as_text in
  let dir_str, distance_str =
    ( String.sub instruction_as_text 0 1,
      String.sub instruction_as_text 1 (instruction_as_text_length - 1) )
  in
  int_of_string_opt distance_str
  |> Option.map (fun distance ->
         match dir_str with
         | "L" -> Some (Left, distance)
         | "R" -> Some (Right, distance)
         | _ -> None)
  |> Option.join

let apply_instruction ((direction, distance) : instruction) (base_number : int)
    =
  let raw_number =
    match direction with
    | Left -> base_number - distance
    | Right -> base_number + distance
  in
  raw_number mod 100

let () =
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
