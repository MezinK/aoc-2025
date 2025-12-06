open Advent

let get_range s =
  let s, e =
    match String.split_on_char '-' s with
    | [ a; b ] -> (int_of_string a, int_of_string b)
    | _ -> failwith "parse_range"
  in
  (s, e)

let get_fresh_ids_and_ingredients input =
  let idx =
    match List.find_index (( = ) "") input with
    | None -> failwith "empty str index not found"
    | Some i -> i
  in
  let ranges = List.take idx input in
  let ingredients = List.drop (idx + 1) input in
  let ranges' = List.map get_range ranges in
  let ingredients' = ingredients |> List.map int_of_string in

  (ranges', ingredients')

let get_fresh_id_ranges input =
  let idx =
    match List.find_index (( = ) "") input with
    | None -> failwith "empty str index not found"
    | Some i -> i
  in
  let ranges = List.take idx input in
  List.map get_range ranges

let in_any_range x ranges = List.exists (fun (s, e) -> x >= s && x <= e) ranges

let merge_ranges ranges =
  let ranges = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) ranges in
  let rec loop acc = function
    | [] -> acc
    | (s, e) :: tl -> (
        match acc with
        | [] -> loop [ (s, e) ] tl
        | (cs, ce) :: tl' ->
            if s <= ce + 1 then
              let ce' = max ce e in
              loop ((cs, ce') :: tl') tl
            else loop ((s, e) :: acc) tl)
  in
  loop [] ranges

let count_ids_in_range range = List.map (fun (s, e) -> e - s + 1) range
let day = 5

let part1 input =
  let fresh_id_ranges, ingredients = get_fresh_ids_and_ingredients input in
  List.fold_left
    (fun acc x -> if in_any_range x fresh_id_ranges then acc + 1 else acc)
    0 ingredients
  |> string_of_int

let part2 input =
  input |> get_fresh_id_ranges |> merge_ranges |> count_ids_in_range |> Math.sum
  |> string_of_int
