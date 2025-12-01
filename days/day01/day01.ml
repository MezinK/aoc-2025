let day = 1

let part1 input =
  let process (pos, found) item =
    let dir = String.get item 0 in
    let number = int_of_string (String.sub item 1 (String.length item - 1)) in
    let step = number mod 100 in
    let new_pos =
      match dir with
      | 'L' -> (pos - step + 100) mod 100
      | 'R' -> (pos + step) mod 100
      | _ -> invalid_arg "no match"
    in
    let hits = if new_pos = 0 then 1 else 0 in
    (new_pos, found + hits)
  in
  let _, found = List.fold_left process (50, 0) input in
  string_of_int found

let part2 input =
  let process (pos, found) item =
    let dir = String.get item 0 in
    let number = int_of_string (String.sub item 1 (String.length item - 1)) in
    let steps_until_zero =
      match dir with
      | 'L' -> if pos = 0 then 100 else pos
      | 'R' -> if pos = 0 then 100 else 100 - pos
      | _ -> invalid_arg "no match"
    in
    let hits =
      if number < steps_until_zero then 0
      else 1 + ((number - steps_until_zero) / 100)
    in
    let step = number mod 100 in
    let new_pos =
      match dir with
      | 'L' -> (pos - step + 100) mod 100
      | 'R' -> (pos + step) mod 100
      | _ -> invalid_arg "no match"
    in
    (new_pos, found + hits)
  in
  let _, found = List.fold_left process (50, 0) input in
  string_of_int found
