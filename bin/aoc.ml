open Advent

type flags = { day : int; test : bool }

let usage () =
  Printf.eprintf "Usage: aoc [--test] <day>\n";
  exit 1

let parse_flags () : flags =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let rec loop day test rest =
    match rest with
    | [] -> begin
        match day with Some d -> { day = d; test } | None -> usage ()
      end
    | "--test" :: xs -> loop day true xs
    | x :: xs ->
        let d =
          match int_of_string_opt x with Some v -> v | None -> usage ()
        in
        if day <> None then usage ();
        loop (Some d) test xs
  in
  loop None false args

let () =
  let { test; day } = parse_flags () in
  match Days.get day with
  | None ->
      Printf.eprintf "Day %d is not implemented yet\n" day;
      exit 1
  | Some (module D : Days.S) ->
      let filename = if test then "test.txt" else "input.txt" in
      let input_path = Printf.sprintf "days/day%02d/%s" D.day filename in
      let input = Input.read_lines input_path in
      let r1, t1 = Timer.time_pretty D.part1 input in
      let r2, t2 = Timer.time_pretty D.part2 input in
      Printf.printf "Day %02d - Part 1: %s (took %s)\n" day r1 t1;
      Printf.printf "Day %02d - Part 2: %s (took %s)\n" day r2 t2
