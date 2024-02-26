(** Netkat Automaton *)

type t

(** Produce a string representation of the automaton for output. *)
val to_string : t -> string

(** Convert a Netkat expression to a Netkat automaton by taking Brzozowski
    derivatives *)
val autom : Nk.t -> t

(** Decide whether the two Netkat automaton are bisimilar. Note that because
    the representation forces that the automata are deterministic, this is
    equivalent to deciding language equivalence. *)
val bisim : t -> t -> bool

(** Run the forward algorithm to compute the set of output packets. *)
val forward : Nk.t -> Sp.t

(** Run the backward algorithm to compute the set of input packets that have
    output. *)
val backward : Nk.t -> Sp.t
