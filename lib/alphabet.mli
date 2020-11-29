module type Alphabet = sig

  type symbol
  type t

  val compare : symbol -> symbol -> int

  val iter : (symbol->unit) -> t -> unit

  val extract_json : Yojson.Basic.t -> symbol

  val to_json : symbol -> Yojson.Basic.t

  val to_string : symbol -> string

end
