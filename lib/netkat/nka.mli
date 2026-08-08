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

(** Produces a string representation of the automaton for output. *)
val to_string : t -> string

(** Converts a Netkat expression to a Netkat automaton by taking Brzozowski
    derivatives. *)
val autom : Nk.t -> t

(** Decides whether the given trace is accepted by the automaton. *)
val accept : t -> Trace.t -> bool

(** Returns a trace accepted by this automaton for the given set of fields. Fails
    if the automaton is equivalent to Drop. *)
val rep : t -> Field.S.t -> Trace.t

(** How [simulate_init] should enumerate a given diversify field's values.
    [BestEffort] takes whatever [Sp.diversify_keys] finds locally at each
    state -- which can under-enumerate a field whose live values are
    hidden behind a non-drop default or an as-yet-untested branch at that
    specific state, since there's nothing in the state's own output to
    recover them from. [Exhaustive] instead enumerates against that
    field's full set of values anywhere reachable from [simulate_init]'s
    own [init], computed once up front rather than per state. [Explicit
    vs] enumerates against the caller-supplied [vs] instead of a computed
    domain. *)
type diversify_mode = BestEffort | Exhaustive | Explicit of Value.S.t

(** [simulate_init ?max_rounds a init modes fields] explores states
    reachable from the symbolic input [init], unrolling self-loops (e.g.
    the [net = (hop.dup)*] shape) round by round rather than stopping
    after the first traversal, and returns one representative concrete
    trace per behaviorally-distinct branch of every state's own output at
    every round, rather than a single witness ([rep]) or a summarized
    output set ([forward_init]). For each state examined, its own output
    is enumerated via [Sp.rep_over] over the fields named in [modes], so
    those fields contribute one trace per branch (bounding the growth to
    the product of branching factors of exactly those fields, per their
    own [diversify_mode]) while every other field picks a single
    arbitrary representative, as [rep] does. Exploration stops once a
    round's witnesses add nothing new (after collapsing consecutive
    duplicate packets), or after [max_rounds] rounds (default 50),
    whichever comes first -- the latter is a genuine safety net for real
    cycles in the underlying topology graph, not just a formality. *)
val simulate_init : ?max_rounds:int -> t -> Sp.t -> diversify_mode Field.M.t -> Field.S.t -> Trace.t list

(** [simulate a fields] is [simulate_init a Sp.skip Field.M.empty fields]. *)
val simulate : t -> Field.S.t -> Trace.t list

(** [forward_over a init] is the union, over every state of [a], of the
    portion of that state's own output reachable from [init] -- the
    automaton-native analogue of [forward_init], computed directly from
    [a]'s own [states]/[trans]/[obs] rather than by re-deriving them from
    an [Nk.t] expression. An unconditional fixed point (no bound needed):
    [a]'s state space is already finite. Used by [simulate_init] to build
    the domain for an [Exhaustive] field without needing the original
    [Nk.t] expression [a] was built from. *)
val forward_over : t -> Sp.t -> Sp.t

(** Computes a trace in the symmetric difference of the trace sets for the two
    automata. If the automata are language equivalent, returns [None]. *)
val xor_rep : t -> t -> Field.S.t -> Trace.t option

(** Decides whether the two Netkat automaton are bisimilar. Because
    the representation forces that the automata are deterministic, this is
    equivalent to deciding language equivalence. *)
val bisim : t -> t -> bool

(** Runs the forward algorithm to compute the set of output packets. *)
val forward : Nk.t -> Sp.t
val forward_init : Nk.t -> Sp.t -> Sp.t

(** Runs the backward algorithm to compute the set of input packets that have
    output. *)
val backward : Nk.t -> Sp.t
val backward_final : Nk.t -> Sp.t -> Sp.t

(** Computes the size of an automaton in the form [n, m] where [n] is the number
    of automaton states, and [m] is the sum of the sizes of the transition and
    observation function SPPs. *)
val size : t -> int * int

(** Returns the smaller of two automata, with respect to [size]. *)
val min : t -> t -> t
