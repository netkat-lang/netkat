(* From the paper, a netkat automaton consists of:
States A set of states 𝑄.
Initial state A state 𝑞0 ∈ 𝑄.
Transitions A function 𝛿 : 𝑄 × 𝑄 → SPP.
Output A function 𝜖 : 𝑄 → SPP
*)

type state = Nkexp.t

module StatePair = struct
  type t = Nkexp.t * Nkexp.t
  let compare (a,b) (c,d) = if Nkexp.eq a c then Nkexp.compare b d else Nkexp.compare a c 
end

module StateSet = Set.Make(Nkexp)
module PairSet = Set.Make(StatePair)
module StateMap = Map.Make(Nkexp)
module TransMap = Map.Make(StatePair)

type t = {
  states: StateSet.t;
  start: state;
  trans: Spp.t TransMap.t;
  obs: Spp.t StateMap.t;
}

let autom (e: Nkexp.t) : t =
  let rec loop (q: Nkexp.t list) (discovered: StateSet.t) tr ob = match q with
  | [] -> {
    states = discovered;
    start = e;
    trans = tr;
    obs = ob;
  }
  | e0::rem -> if StateSet.mem e0 discovered then
                 loop rem discovered tr ob
               else
                 let sts = Deriv.d e0 in
                 let ob' = StateMap.add e0 (Deriv.e e0) ob in
                 let (q',disc,tr') = List.fold_left (fun (nq,d,t) (ei, sppi) ->
                   if StateSet.mem ei discovered then
                      (nq,d,t)
                   else
                      (
                        ei::nq,
                        StateSet.add ei d,
                        TransMap.add (e0, ei) sppi t
                      )) (rem,discovered,tr) (Sts.to_list sts) in
                 loop rem disc tr' ob' in
  loop [e] StateSet.empty TransMap.empty StateMap.empty

let bisim (a1: t) (a2: t) : bool =
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem -> if not (Sp.eq (Spp.push pk (StateMap.find s1 a1.obs))
                                       (Spp.push pk (StateMap.find s2 a2.obs))) then
                           false
                         else
                           let q' = failwith "x" in
                           let visited' = failwith "y" in
                           bq q' visited'
  in bq [(Sp.skip, a1.start, a2.start)] StateSet.empty
