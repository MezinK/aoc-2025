open Advent

let day = 2
let range start stop = List.init (stop - start + 1) (fun x -> start + x)
let expand (a, b) = range a b

let parse_interval interval =
  match interval with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> invalid_arg "parse_interval failed"

let parse_and_expand input =
  input |> Input.handle_comma_input
  |> List.map (String.split_on_char '-')
  |> List.map parse_interval |> List.concat_map expand

let all_chars_equal s =
  let len = String.length s in
  len > 1
  &&
  let c0 = s.[0] in
  let rec loop i =
    if i = len then true else if c0 <> s.[i] then false else loop (i + 1)
  in
  loop 1

let is_repetition s k =
  let len = String.length s in
  let rec loop i =
    if i = len then true else if s.[i] <> s.[i - k] then false else loop (i + 1)
  in
  loop k

let is_id_invalid n =
  let s = string_of_int n in
  let len = String.length s in
  len mod 2 = 0
  &&
  let half = len / 2 in
  let rec loop i =
    if i = half then true
    else if s.[i] <> s.[i + half] then false
    else loop (i + 1)
  in
  loop 0

let is_id_invalid2 n =
  let s = string_of_int n in
  let len = String.length s in
  if len <= 1 then false
  else if all_chars_equal s then true
  else
    let rec try_repetition k =
      if k > len / 2 then false
      else if len mod k <> 0 then try_repetition (k + 1)
      else if is_repetition s k then true
      else try_repetition (k + 1)
    in
    try_repetition 2

let part1 input =
  input |> parse_and_expand |> List.filter is_id_invalid |> Math.sum
  |> string_of_int

let part2 input =
  input |> parse_and_expand |> List.filter is_id_invalid2 |> Math.sum
  |> string_of_int
