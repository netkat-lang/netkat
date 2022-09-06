open Alphabet

module type D = sig

  type symbol

  module Nfa : Nfa.N with type state = int and type character = symbol

  type state = Nfa.state

  module CharMap : Map.S with type key = symbol

  type t = {
    start : state;
    final : Nfa.StateSet.t;
    transition : (state CharMap.t) Nfa.StateMap.t
  }

  val new_state : t -> state -> state

  val dfa_to_nfa : t -> Nfa.t

  val to_string : symbol list -> string

  val mk_dfa : int -> (int*symbol*int) list -> state list -> t

  val json_to_dfa : Yojson.Basic.t -> t

  val dfa_to_json : t ->
    [> `Assoc of (string * [> `Int of state |
                              `List of Yojson.Basic.t list ]) list ]

  val dfa_to_channel : t -> out_channel -> unit
  val dump_json : t -> unit
  val dump_latex : t -> unit
  val get_alphabet : t -> symbol list
  val get_states : t -> Nfa.StateSet.t
  val size : t -> int
  val determinize : Nfa.t -> t
  val find_counterexample : t -> t -> symbol option list option
  val minimize_dfa : t -> t
  val equivalence : t -> t -> bool
  val rep_symlist : t -> symbol list option
  val representative : t -> string
  val accepts : t -> symbol list -> bool
  val validate : t -> symbol list list -> symbol list list -> bool
  val enum_bin_dfas : int -> t list
end

module MakeDfa: 
  functor (A : Alphabet) -> D with type symbol = A.symbol
