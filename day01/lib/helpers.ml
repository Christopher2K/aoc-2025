type instruction_direction = Left | Right
type instruction = instruction_direction * int

let start_number = 50

let read_input () =
  In_channel.open_text "./bin/input.txt" |> In_channel.input_lines

let read_test_input () =
  In_channel.open_text "./bin/input_test.txt" |> In_channel.input_lines

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
