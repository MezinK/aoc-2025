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

  let curr = ref (IntSet.singleton start_col) in
  let splits = ref 0 in
  for r = start_row + 1 to rows - 1 do
    let next = ref IntSet.empty in
    let seen = ref IntSet.empty in
    let queue = ref (IntSet.elements !curr) in
    while !queue <> [] do
      let c = List.hd !queue in
      queue := List.tl !queue;
      if c >= 0 && c < cols && not (IntSet.mem c !seen) then begin
        seen := IntSet.add c !seen;
        match grid.(r).(c) with
        | '^' ->
            incr splits;
            queue := (c + 1) :: (c - 1) :: !queue
        | _ -> next := IntSet.add c !next
      end
    done;
    curr := !next
  done;

  !splits

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
