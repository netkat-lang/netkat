type symbol = int


let compare = Stdlib.compare

type t = int list

let iter (f: symbol->unit) (alph: t) = List.iter f alph
