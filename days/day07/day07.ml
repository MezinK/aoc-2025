module IntSet = Set.Make (Int)

let parse_grid input =
  let rows = List.length input in
  let cols = String.length (List.hd input) in
  let grid = Array.make_matrix rows cols '.' in
  List.iteri
    (fun r s ->
      for c = 0 to cols - 1 do
        grid.(r).(c) <- s.[c]
      done)
    input;
  grid

let get_start_col r grid =
  match Array.find_index (( = ) 'S') grid.(r) with
  | Some col -> col
  | None -> failwith "Start column not found"

let count_splits grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let start_row = 0 in
  let start_col = get_start_col start_row grid in

  let rec bfs r acc seen next = function
    | [] -> (next, acc)
    | c :: queue -> (
        if c < 0 || c >= cols || IntSet.mem c seen then
          bfs r acc seen next queue
        else
          let seen = IntSet.add c seen in
          match grid.(r).(c) with
          | '^' ->
              let queue = (c + 1) :: (c - 1) :: queue in
              bfs r (acc + 1) seen next queue
          | _ ->
              let next = IntSet.add c next in
              bfs r acc seen next queue)
  in

  let rec loop r curr acc =
    if r = rows then acc
    else
      let queue = IntSet.elements curr in
      let next, acc = bfs r acc IntSet.empty IntSet.empty queue in
      loop (r + 1) next acc
  in
  let init = IntSet.singleton start_col in
  loop start_row init 0

let count_timelines grid =
  let rows = Array.length grid in
  let start_row = 0 in
  let start_col = get_start_col start_row grid in
  let memo = Hashtbl.create 10000 in
  let rec dfs (r, c) =
    if r == rows then 1
    else
      match Hashtbl.find_opt memo (r, c) with
      | Some m -> m
      | None ->
          let res =
            match grid.(r).(c) with
            | '.' -> dfs (r + 1, c)
            | '^' -> dfs (r + 1, c + 1) + dfs (r + 1, c - 1)
            | _ -> failwith (Printf.sprintf "dfs failed on row: %d col: %d" r c)
          in
          Hashtbl.add memo (r, c) res;
          res
  in
  dfs (start_row + 1, start_col)

let day = 7
let part1 input = input |> parse_grid |> count_splits |> string_of_int
let part2 input = input |> parse_grid |> count_timelines |> string_of_int
