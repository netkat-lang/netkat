
type state = int32

module type Alphabet = sig

  type t

  val compare : t -> t -> int

end