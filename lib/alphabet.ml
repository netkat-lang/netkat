module type Alphabet = sig

  type symbol
  type t

  val compare : symbol -> symbol -> int

  val iter : (symbol->unit) -> t -> unit

  val extract_json : Yojson.Basic.t -> symbol

  val to_json : symbol -> Yojson.Basic.t

  val to_string : symbol -> string

end

module A : Alphabet = struct

  type symbol = int

  let compare = Stdlib.compare

  type t = int list

  let iter (f: symbol->unit) (alph: t) = List.iter f alph

  let extract_json = function
    | `Int i -> i
    | _ -> failwith "invalid json format"

  let to_json i = `Int i

  let to_string = string_of_int

end