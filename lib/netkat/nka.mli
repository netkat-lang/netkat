(** Representation of a NetKAT automaton.

A NetKAT automaton consists of: 

{ul
  {- States: A set of states 𝑄}
  {- Initial state: A state 𝑞0 ∈ 𝑄.}
  {- Transitions: A function 𝛿 : 𝑄 × 𝑄 → SPP.}
  {- Output: A function 𝜖 : 𝑄 → SPP}}
*)

(** Representation of a state in a NetKAT automaton. *)
module State : sig
    type t = int
    val compare : t -> t -> int
    val eq : t -> t -> bool
    val to_string : t -> string
    val drop : t
  end
  
module StateMap : Map.S with type key = State.t
module StateSet : Set.S with type elt = State.t

(** The representation of a NetKAT automaton as described above. *)
type t = {
  states: StateSet.t;
  start: State.t;
  trans: Spp.t StateMap.t StateMap.t;
  obs: Spp.t StateMap.t;
}

(** Produce a string representation of the automaton for output. *)
val to_string : t -> string

(** Convert a Netkat expression to a Netkat automaton by taking Brzozowski
    derivatives *)
val autom : Nk.t -> t

(** Decide whether the given trace is accepted by the automaton. *)
val accept : t -> Trace.t -> bool

(** Return a trace accepted by this automaton for the given set of fields. Fail
    if the automaton is equivalent to Drop. *)
val rep : t -> Field.S.t -> Trace.t

(** Compute a trace in the symmetric difference of the trace sets for the two
    automata. If the automata are language equivalent, return None. *)
val xor_rep : t -> t -> Field.S.t -> Trace.t option

(** Decide whether the two Netkat automaton are bisimilar. Because
    the representation forces that the automata are deterministic, this is
    equivalent to deciding language equivalence. *)
val bisim : t -> t -> bool

(** Run the forward algorithm to compute the set of output packets. *)
val forward : Nk.t -> Sp.t

(** Run the backward algorithm to compute the set of input packets that have
    output. *)
val backward : Nk.t -> Sp.t

(** Compute the size of an automaton in the form [n, m] where [n] is the number
    of automaton states, and [m] is the sum of the sizes of the transition and
    observation function SPPs. *)
val size : t -> int * int

(** Return the smaller of two automata, with respect to [size] *)
val min : t -> t -> t
