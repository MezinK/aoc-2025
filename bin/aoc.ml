open Advent

let usage () =
  Printf.eprintf "Usage: aoc <day>\n";
  exit 1

let () =
  if Array.length Sys.argv <> 2 then usage ();

  let day = try int_of_string Sys.argv.(1) with _ -> usage () in

  match Days.get day with
  | None ->
      Printf.eprintf "Day %d is not implemented yet\n" day;
      exit 1
  | Some (module D : Days.S) ->
      let input_path = Printf.sprintf "days/day%02d/input.txt" D.day in
      let input = Input.read_lines input_path in
      let r1, t1 = Timer.time_pretty D.part1 input in
      let r2, t2 = Timer.time_pretty D.part2 input in
      Printf.printf "Day %02d - Part 1: %s (took %s)\n" day r1 t1;
      Printf.printf "Day %02d - Part 2: %s (took %s)\n" day r2 t2
