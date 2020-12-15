open Alphabet

module MakeNfa (A : Alphabet) : sig

  module IntOrdered : sig

    type t = int

    val compare : t -> t -> int

  end

  module CharOrdered : sig

    type t = Empty | Char of A.symbol

    val compare : t -> t -> int
  end

  module States : Set.S with type elt = IntOrdered.t
  module StateMap : Map.S with type key = IntOrdered.t
  module CharMap : Map.S with type key = CharOrdered.t

  type t = {
    start : States.t;
    final : States.t;
    transition : (States.t CharMap.t) StateMap.t
  }

  val empty : t -> bool

  val union : t -> t -> t

  val kleene : t -> t

  val concatenation : t -> t -> t

  val accept : t -> CharOrdered.t list -> bool 

end

(*)   val intersection : t -> t -> t

   val equivalence : t -> t -> bool*)