module type Alphabet = sig

  type symbol
  type t

  val alphabet : t

  val compare : symbol -> symbol -> int

  val iter : (symbol->unit) -> t -> unit
  val fold :  ('a->symbol->'a) -> 'a -> 'a

  val extract_json : Yojson.Basic.t -> symbol

  val to_json : symbol -> Yojson.Basic.t

  val to_string : symbol -> string

end

module A : Alphabet = struct

  type symbol = int

  let compare = Stdlib.compare

  type t = int list

  let alphabet: t = [0 ; 1]

  let iter (f: symbol->unit) (alph: t) = List.iter f alph

  let fold (f:'a->symbol->'a) (a:'a) = List.fold_left f a alphabet

  let extract_json = function
    | `Int i -> i
    | _ -> failwith "invalid json format"

  let to_json i = `Int i

  let to_string = string_of_int

end
