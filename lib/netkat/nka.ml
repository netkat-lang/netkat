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
module PairMap = Map.Make(StatePair)
module StateMap = Map.Make(Nkexp)

type t = {
  states: StateSet.t;
  start: state;
  trans: (Spp.t StateMap.t) StateMap.t;
  obs: Spp.t StateMap.t;
}


let to_string (a: t) =
  let ebinding_to_string ((e,p): Nkexp.t * Spp.t) =
    (Nkexp.to_string e) ^ "↦(" ^ (Spp.to_string p) ^ ")" in
  let tbinding_to_string ((e,m): Nkexp.t * (Spp.t StateMap.t)) =
    (Nkexp.to_string e) ^ " ↦ [" ^ (StateMap.bindings m |> List.map ebinding_to_string |> String.concat ", ") ^ "]" in
  "states: " ^ (StateSet.elements a.states |> List.map Nkexp.to_string |> String.concat ", ") ^
  "\nstart: " ^ (Nkexp.to_string a.start) ^
  "\ntrans: " ^ (StateMap.bindings a.trans |> List.map tbinding_to_string |> String.concat "; ") ^
  "\nobs: " ^ (StateMap.bindings a.obs |> List.map ebinding_to_string  |> String.concat ", ") ^ "\n\n"

let autom (e: Nkexp.t) : t =
  (* Add a transition from [e0] to [e1] along [spp]. The trick is that SPPs
     transitions out of e0 need to be disjoint, so we have to go through and
     update other transitions to maintain the invariant.
     Precondition: [tr] contains a binding for [e0]. *)
  let lookup e m = match StateMap.find_opt e m with
                 | None -> Spp.drop
                 | Some spp -> spp in
  let add_nonempty e s m =
    if Spp.eq s Spp.drop then m else StateMap.add e (Spp.union_pair (lookup e m) s) m in
  let add_trans (tr: Spp.t StateMap.t StateMap.t) (e0: Nkexp.t) (e1: Nkexp.t) (spp: Spp.t) =
    if Spp.eq spp Spp.drop then tr else
    let e0map = StateMap.find e0 tr in
    let (e0map',s) = List.fold_left (fun (m,s) (ei,sppi) ->
      let inter = Spp.intersect_pair s sppi in
      let diff = Spp.diff sppi s in
      let s' = Spp.diff s sppi in
      (add_nonempty (Nkexp.union_pair e1 ei) inter m
      |> add_nonempty ei diff, s')
    ) (StateMap.empty, spp) (StateMap.bindings e0map) in
    let e0map'' = add_nonempty e1 s e0map' in
    StateMap.add e0 e0map'' tr in

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
                 let disc = StateSet.add e0 discovered in
                 let sts = Deriv.d e0 in
                 let ob' = StateMap.add e0 (Deriv.e e0) ob in
                 let tr0 = StateMap.add e0 (StateMap.singleton Nkexp.drop Spp.skip) tr in
                 let (q',tr') = List.fold_left (fun (nq,t) (ei, sppi) ->
                      (
                        ei::nq,
                        add_trans t e0 ei sppi
                      )) (rem,tr0) (Sts.to_list sts) in
                 loop (q'@rem) disc tr' ob' in
  loop [e; Nkexp.drop] StateSet.empty StateMap.empty StateMap.empty

let bisim (a1: t) (a2: t) : bool =
  (* let () = Printf.printf "bisim let's goooooo\na1:\n%s\na2:\n%s\n" (to_string a1) (to_string a2) in *)
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem -> let () = () in
                         (* let () = Printf.printf "comparing %s %s (for pk=%s)\n%!" (Nkexp.to_string s1) (Nkexp.to_string s2) (Sp.to_string pk) in *)
                         if Sp.eq pk Sp.drop ||
                            (PairMap.mem (s1,s2) visited) && 
                            (Sp.le pk (PairMap.find (s1,s2) visited)) then
                           (* let () = Printf.printf "%s\n%!" __LOC__ in *)
                           bq rem visited
                         else if not (Spp.eq (Spp.seq_pair (Spp.of_sp pk) (StateMap.find s1 a1.obs))
                                             (Spp.seq_pair (Spp.of_sp pk) (StateMap.find s2 a2.obs))) then
                           false
                         else
                           let tr1 = StateMap.find s1 a1.trans |> StateMap.bindings in
                           let tr2 = StateMap.find s2 a2.trans |> StateMap.bindings in
                           let next = List.fold_left (fun a (ei, sppi)->
                              (List.map (fun (ej, sppj) ->
                                let pk' = Sp.intersect_pair (Spp.push pk sppi) (Spp.push pk sppj) in
                                (pk', ei, ej)) tr2)@a) [] tr1 in
                           let visited' = PairMap.add (s1,s2) pk visited in
                           bq next visited'
  in bq [(Sp.skip, a1.start, a2.start)] PairMap.empty
