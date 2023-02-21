type symbol = Alphabet.symbol
type word = Alphabet.word

type t
type state

module StateSet : Set.S with type elt = int

(* Provides an interface to mk_dfa:
   'a is the type of state
   [eq s0 s1] Returns true if s0 and s1 are the same state.
   [d a q]    Returns the derivative of q wrt a; that is, the state we should
              transition to on a from q.
   [e q]      Returns true if q is an accepting state. *)
type 'a regular = { eq: 'a -> 'a -> bool;
                    d: symbol -> 'a -> 'a;
                    e: 'a -> bool }

val mk_dfa : 'a regular -> Alphabet.t -> 'a -> t

val compare_states : state -> state -> int
val size : t -> int
val get_alpha : t -> Alphabet.t
val get_start : t -> state
val step : t -> state -> symbol -> state
val accepting : t -> state -> bool
val accept : t -> word -> bool
val validate : t -> word list -> word list -> bool
val is_empty : t -> bool
val rep : t -> word
val complement : t -> t
val print : t -> unit

module type Determ  = sig
  module N : Nfa.N
  val determinize : N.t -> t
end

module Determinizer:
  functor (S : Nfa.State) -> Determ

val to_nfa : t -> IntNfa.t
val minimize : t -> t
val of_rx : Alphabet.t -> Rx.t -> t
val to_rx : t -> Rx.t

val union : t -> t -> t
val intersect : t -> t -> t
val diff : t -> t -> t
val symdiff : t -> t -> t
val equiv : t -> t -> bool
