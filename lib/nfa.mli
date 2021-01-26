open Alphabet
open Dfa

module MakeNfa (A : Alphabet) : sig

  module CharOrdered : sig

    type t = A.symbol option

    val compare : t -> t -> int
  end

  module StateSet : Set.S with type elt = Int.t
  module StateMap : Map.S with type key = Int.t
  module CharMap : Map.S with type key = CharOrdered.t

  type t = {
    start : StateSet.t;
    final : StateSet.t;
    transition : (StateSet.t CharMap.t) StateMap.t
  }

  val empty : t -> bool

  val union : t -> t -> t

  val kleene : t -> t

  val concatenation : t -> t -> t

  val accept : t -> CharOrdered.t list -> bool 

  val intersection : t -> t -> t

  val epsilon_remove : t -> t

  val equivalence : t -> t -> bool

end
