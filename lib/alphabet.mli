module type Alphabet = sig

  type symbol
  type t

  val alphabet: t

  val compare : symbol -> symbol -> int

  val iter : (symbol->unit) -> t -> unit
  val fold :  ('a->symbol->'a) -> 'a -> 'a

  val extract_json : Yojson.Basic.t -> symbol

  val to_json : symbol -> Yojson.Basic.t

  val to_string : symbol -> string

end
