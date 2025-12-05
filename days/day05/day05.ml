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
  let ranges_lst = List.take (idx - 1) input in
  let ingredients_lst = List.drop (idx + 1) input in
  let ranges = List.map get_range ranges_lst in
  let ingredients = ingredients_lst |> List.map int_of_string in

  (ranges, ingredients)

(*tbd*)
let get_fresh_id_ranges input =
  let idx =
    match List.find_index (( = ) "") input with
    | None -> failwith "empty str index not found"
    | Some i -> i
  in
  let ranges_lst = List.take (idx - 1) input in
  let ranges = List.map get_range ranges_lst in
  List.fold_left (fun acc (s, e) -> acc + e - s + 1) 0 ranges

let in_any_range x ranges = List.exists (fun (s, e) -> x >= s && x <= e) ranges
let day = 5

let part1 input =
  let fresh_id_ranges, ingredients = get_fresh_ids_and_ingredients input in
  List.fold_left
    (fun acc x -> if in_any_range x fresh_id_ranges then acc + 1 else acc)
    0 ingredients
  |> string_of_int

(*tbd*)
let part2 input = input |> get_fresh_id_ranges |> string_of_int
