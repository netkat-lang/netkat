(*
open Yojson.Basic
open Yojson.Basic.Util
*)
type symbol = Alphabet.symbol
type word = Alphabet.word

(* Transitions are labeled by a symbol or epsilon *)
type nsymbol = Char of symbol | Eps

module type State = sig
  type t
  module StateSet : Set.S with type elt = t
  val compare : t -> t -> int
  val to_string : t -> string
  val fresh : StateSet.t -> t
end

module type N = sig
 
  type state

  module StateSet : Set.S with type elt = state
  module StateMap : Map.S with type key = state
  module CharMap : Map.S with type key = nsymbol

  type t

  val mk_nfa : Alphabet.t -> state list -> state list -> (state*nsymbol*state) list -> t
  val get_alpha : t -> Alphabet.t
  val get_start : t -> StateSet.t
  val contains_final : t -> StateSet.t -> bool
  val accept : t -> word -> bool
  val next : t -> StateSet.t -> symbol -> StateSet.t
  val trans_list : t -> (state * nsymbol * state) list
  val reverse : t -> t
  val print : t -> unit
  val to_rx : t -> Rx.t
  (*
  rewrite / adapt:
  val json_to_dfa : Yojson.Basic.t -> t

  val dfa_to_json : t ->
    [> `Assoc of (string * [> `Int of state |
                            `List of Yojson.Basic.t list ]) list ]
                            *)
  (*
  val union : t -> t -> t

  val kleene : t -> t

  val concatenation : t -> t -> t

  val intersection : t -> t -> t

  val to_dot : t -> string -> unit
  *)
end

module Make: 
  functor (S : State) -> N with type state = S.t
