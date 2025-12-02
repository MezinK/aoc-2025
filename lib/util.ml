let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

let handle_comma_input = List.concat_map (String.split_on_char ',')

let time f x =
  let start = Unix.gettimeofday () in
  let res = f x in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let format_time t =
  if t < 1e-6 then Printf.sprintf "%.2fns" (t *. 1e9)
  else if t < 1e-3 then Printf.sprintf "%.2fµs" (t *. 1e6)
  else if t < 1.0 then Printf.sprintf "%.2fms" (t *. 1e3)
  else Printf.sprintf "%.2fs" t

let time_pretty f x =
  let res, elapsed = time f x in
  (res, format_time elapsed)
