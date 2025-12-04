open Advent

type candidate = { num : int; pos : int }

let max_candidate a b =
  match compare a.num b.num with
  | 0 -> if a.pos <= b.pos then a else b
  | c when c > 0 -> a
  | _ -> b

let find_max = List.fold_left max_candidate { num = -1; pos = -1 }
let char_to_candidate i c = { num = int_of_char c - int_of_char '0'; pos = i }

let string_to_candiates s =
  List.init (String.length s) (fun i -> char_to_candidate i s.[i])

let find_highest_k_digits k c =
  let rec loop n digits acc =
    let digit_count = List.length digits in
    let pick_range = digit_count - n + 1 in
    if n = 0 then List.rev acc
    else
      let digits_in_range = List.take pick_range digits in
      let max_digit_in_range = find_max digits_in_range in
      let digits' = List.drop (max_digit_in_range.pos + 1) c in
      loop (n - 1) digits' (max_digit_in_range :: acc)
  in
  loop k c []

let candidate_to_int = List.fold_left (fun acc d -> (acc * 10) + d.num) 0
let day = 3

let part1 input =
  let candidates = input |> List.map string_to_candiates in
  let candidates_without_last_item =
    List.map (fun xs -> List.take (List.length xs - 1) xs) candidates
  in
  let first_maxes = List.map find_max candidates_without_last_item in
  let candidates' =
    List.map2
      (fun max row -> List.drop (max.pos + 1) row)
      first_maxes candidates
  in
  let second_maxes = List.map find_max candidates' in
  List.map2 (fun m1 m2 -> (10 * m1.num) + m2.num) first_maxes second_maxes
  |> Math.sum |> string_of_int

let part2 input =
  input
  |> List.map string_to_candiates
  |> List.map (fun x -> find_highest_k_digits 12 x)
  |> List.map candidate_to_int |> Math.sum |> string_of_int
