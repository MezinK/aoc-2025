module type S = sig
  val day : int
  val part1 : string list -> string
  val part2 : string list -> string
end

let registry = [ (1, (module Day01 : S)) ]
let get day = List.assoc_opt day registry
