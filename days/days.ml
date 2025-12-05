module type S = sig
  val day : int
  val part1 : string list -> string
  val part2 : string list -> string
end

let registry =
  [
    (1, (module Day01 : S));
    (2, (module Day02 : S));
    (3, (module Day03 : S));
    (4, (module Day04 : S));
    (5, (module Day05 : S));
  ]

let get day = List.assoc_opt day registry
