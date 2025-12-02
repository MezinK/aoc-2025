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

let split_into_chunks k s =
  if k = 0 then invalid_arg "split_into_chunk k > 0"
  else
    let len = String.length s in
    let rec loop i acc =
      if i >= len then List.rev acc
      else
        let take = min k (len - i) in
        loop (i + take) (String.sub s i take :: acc)
    in
    loop 0 []

let all_equal l =
  match l with [] | [ _ ] -> false | x :: xs -> List.for_all (( = ) x) xs

let is_id_invalid n =
  let s = string_of_int n in
  let len = String.length s in
  len mod 2 = 0
  &&
  let half = len / 2 in
  String.sub s 0 half = String.sub s half half

let is_id_invalid2 n =
  let s = string_of_int n in
  let len = String.length s in
  if all_equal (split_into_chunks 1 s) then true
  else
    List.init (len / 2) (fun x -> x + 2)
    |> List.filter (fun k -> len mod k = 0)
    |> List.exists (fun k -> all_equal (split_into_chunks k s))

let part1 input =
  input |> parse_and_expand |> List.filter is_id_invalid |> Math.sum
  |> string_of_int

let part2 input =
  input |> parse_and_expand |> List.filter is_id_invalid2 |> Math.sum
  |> string_of_int
