(* From the paper, a netkat automaton consists of:
States A set of states 𝑄.
Initial state A state 𝑞0 ∈ 𝑄.
Transitions A function 𝛿 : 𝑄 × 𝑄 → SPP.
Output A function 𝜖 : 𝑄 → SPP
*)

type state = Nkexp.t

module StateSet = Set.Make(Nkexp)
module StateMap = Map.Make(Nkexp)
module TransMap = Map.Make(struct
  type t = Nkexp.t * Nkexp.t
  let compare (a,b) (c,d) = if Nkexp.eq a c then Nkexp.compare b d else Nkexp.compare a c 
end)

type t = {
  states: StateSet.t;
  start: state;
  trans: Spp.t TransMap.t;
  obs: Spp.t StateMap.t;
}

let autom (e: Nkexp.t) : t = failwith "TODO"

let bisim (a1: t) (a2: t) : bool = failwith "TODO"
