type symbol = int

let compare = Stdlib.compare

type t = int list

let alphabet = [0 ; 1]

let iter (f: symbol->unit) (alph: t) = List.iter f alph

let fold (f:'a->symbol->'a) (a:'a) = List.fold_left f a alphabet

let extract_json = function
  | `Int i -> i
  | _ -> failwith "invalid json format"

let to_json i = `Int i

let to_string = string_of_int
