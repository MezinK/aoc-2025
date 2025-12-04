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

let day = 3

let part1 input =
  let candidates = input |> List.map string_to_candiates in
  let candidates_without_last_item =
    List.map (fun xs -> List.take (List.length xs - 1) xs) candidates
  in
  let first_maxes = List.map find_max candidates_without_last_item in
  let new_candidates =
    List.map2
      (fun max row -> List.drop (max.pos + 1) row)
      first_maxes candidates
  in
  let second_maxes = List.map find_max new_candidates in
  List.map2 (fun m1 m2 -> (10 * m1.num) + m2.num) first_maxes second_maxes
  |> Math.sum |> string_of_int

let part2 _ = ""
