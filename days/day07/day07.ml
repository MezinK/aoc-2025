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

let solve grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let start_row = 0 in
  let start_col =
    match Array.find_index (( = ) 'S') grid.(start_row) with
    | Some col -> col
    | None -> failwith "Start column not found"
  in

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

let day = 7
let part1 input = input |> parse_grid |> solve |> string_of_int
let part2 _ = ""
