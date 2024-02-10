
type ip = int * int * int * int
type prefix = ip * int

type t = prefix * ip

let to_nk (rt: t) : Nkexp.t = failwith ("TODO: " ^ __LOC__)
