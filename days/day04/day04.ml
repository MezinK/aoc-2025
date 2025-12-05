let parse_grid input =
  input
  |> List.map (fun row -> Array.init (String.length row) (String.get row))
  |> Array.of_list

let in_bounds grid r c =
  r >= 0 && c >= 0 && r < Array.length grid && c < Array.length grid.(0)

let get_neigbours grid r c =
  let deltas =
    [ (-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  deltas
  |> List.filter_map (fun (dr, dc) ->
      let r' = r + dr in
      let c' = c + dc in
      if in_bounds grid r' c' then Some (r', c', grid.(r').(c')) else None)

let point_is_accessible grid r c =
  let neighbours = get_neigbours grid r c in
  let occupied_points =
    List.fold_left
      (fun acc (_, _, v) -> if v = '@' then acc + 1 else acc)
      0 neighbours
  in
  occupied_points < 4

let count_accessible_rolls grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let count = ref 0 in
  for r = 0 to h - 1 do
    for c = 0 to w - 1 do
      if grid.(r).(c) = '@' && point_is_accessible grid r c then incr count
    done
  done;
  !count

let remove_accessible_rolls grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let track = ref [] in
  let count = ref 0 in
  for r = 0 to h - 1 do
    for c = 0 to w - 1 do
      if grid.(r).(c) = '@' && point_is_accessible grid r c then begin
        incr count;
        track := (r, c) :: !track
      end
    done
  done;

  List.iter (fun (r, c) -> grid.(r).(c) <- '.') !track;
  !count

let remove_until_zero grid =
  let total = ref 0 in
  let removed_count = ref (remove_accessible_rolls grid) in
  total := !total + !removed_count;
  while !removed_count <> 0 do
    removed_count := remove_accessible_rolls grid;
    total := !total + !removed_count
  done;

  !total

let day = 4
let part1 input = input |> parse_grid |> count_accessible_rolls |> string_of_int
let part2 input = input |> parse_grid |> remove_until_zero |> string_of_int
