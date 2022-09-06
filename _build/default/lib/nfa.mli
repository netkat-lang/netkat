open Alphabet

module type N = sig

  type character
  type symbol = character option
  type state = Int.t

  module StateSet : Set.S with type elt = state
  module StateMap : Map.S with type key = state
  module CharMap : Map.S with type key = symbol

  type t = {
    start : StateSet.t;
    final : StateSet.t;
    transition : (StateSet.t CharMap.t) StateMap.t
  }

  val empty : t -> bool

  val union : t -> t -> t

  val kleene : t -> t

  val concatenation : t -> t -> t

  val accept : t -> symbol list -> bool 

  val intersection : t -> t -> t

  val epsilon_remove : t -> t

  val equivalence : t -> t -> bool

  val get_all_states : t -> StateSet.t

  val get_alphabet : t -> symbol list

  val transition_from_char : t -> symbol -> StateSet.t -> StateSet.t

  val to_dot : t -> string -> unit

end

module MakeNfa: 
  functor (A : Alphabet) -> N with type character = A.symbol
