open Advent

let parse_grid input =
  input
  |> List.map (fun row ->
      String.split_on_char ' ' row |> List.filter (( <> ) "") |> Array.of_list)
  |> Array.of_list

let parse_grid2 input =
  let rows = List.length input in
  let max_len =
    List.fold_left (fun acc s -> max acc (String.length s)) 0 input
  in
  let grid = Array.make_matrix rows max_len ' ' in
  List.iteri
    (fun r s ->
      let len = String.length s in
      for c = 0 to len - 1 do
        grid.(r).(c) <- s.[c]
      done)
    input;
  grid

let solve grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let eval_col c =
    let op = grid.(rows - 1).(c) in
    let nums = List.init (rows - 1) (fun r -> int_of_string grid.(r).(c)) in
    match op with
    | "+" -> Math.sum nums
    | "*" -> Math.prod nums
    | _ -> failwith "unknown operator"
  in
  List.init cols eval_col

let solve2 grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let is_seperator_col c =
    let rec loop r =
      if r = rows then true
      else if grid.(r).(c) <> ' ' then false
      else loop (r + 1)
    in
    loop 0
  in

  let extract_number c =
    let buf = Buffer.create rows in
    for r = 0 to rows - 2 do
      let ch = grid.(r).(c) in
      if ch >= '0' && ch <= '9' then Buffer.add_char buf ch
    done;
    int_of_string (Buffer.contents buf)
  in

  let find_operator cls =
    let rec loop = function
      | [] -> failwith "no operator in problem found!"
      | c :: cs ->
          let ch = grid.(rows - 1).(c) in
          if ch = '+' || ch = '*' then ch else loop cs
    in
    loop cls
  in

  let eval cols =
    let op = find_operator cols in
    let nums = List.map extract_number cols in
    match op with
    | '+' -> Math.sum nums
    | '*' -> Math.prod nums
    | _ -> failwith "unknown operator"
  in

  let rec loop c current_cols acc =
    if c < 0 then eval current_cols :: acc
    else if is_seperator_col c then loop (c - 1) [] (eval current_cols :: acc)
    else loop (c - 1) (c :: current_cols) acc
  in
  loop (cols - 1) [] []

let day = 6
let part1 input = input |> parse_grid |> solve |> Math.sum |> string_of_int
let part2 input = input |> parse_grid2 |> solve2 |> Math.sum |> string_of_int
