(** Netkat Automaton *)

module State : sig
  type t = int
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val to_string : t -> string
  val drop : t
end

module StateMap : Map.S with type key = State.t
module StateSet : Set.S with type elt = State.t

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

(** Compute the symmetric difference automaton *)
val xor : t -> t -> t

(** Decide whether the two Netkat automaton are bisimilar. Because
    the representation forces that the automata are deterministic, this is
    equivalent to deciding language equivalence. *)
val bisim : t -> t -> bool

(** Run the forward algorithm to compute the set of output packets. *)
val forward : Nk.t -> Sp.t

(** Run the backward algorithm to compute the set of input packets that have
    output. *)
val backward : Nk.t -> Sp.t
