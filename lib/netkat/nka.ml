(* From the paper, a netkat automaton consists of:
States A set of states 𝑄.
Initial state A state 𝑞0 ∈ 𝑄.
Transitions A function 𝛿 : 𝑄 × 𝑄 → SPP.
Output A function 𝜖 : 𝑄 → SPP
*)
module State = struct
  type t = int
  let compare = Int.compare
  let eq t1 t2 = compare t1 t2 = 0
  let to_string = Int.to_string
  let drop = 0
end

module StatePair = struct
  type t = State.t * State.t
  let compare (a,b) (c,d) = if State.eq a c then State.compare b d else State.compare a c 
end

module StateSet = Set.Make(State)
module PairMap = Map.Make(StatePair)
module StateMap = Map.Make(State)
module NkMap = Map.Make(Nk)

type t = {
  states: StateSet.t;
  start: State.t;
  trans: Spp.t StateMap.t StateMap.t;
  obs: Spp.t StateMap.t;
}

let to_string (a: t) =
  let to_string tr = StateMap.bindings tr
                     |> List.map (fun (s,spp) -> "--[" ^ (Spp.to_string spp) ^ "]-->" ^ (string_of_int s))
                     |> String.concat "; " in
  let ebinding_to_string ((e,p): State.t * Spp.t) =
    (State.to_string e) ^ "↦(" ^ (Spp.to_string p) ^ ")" in
  "States: " ^ (StateSet.elements a.states |> List.map State.to_string |> String.concat ", ") ^
  "\nStart: " ^ (State.to_string a.start) ^
  "\nTrans:\n  " ^ (StateMap.bindings a.trans
  |> List.map (fun (e,tr) -> "  " ^ (State.to_string e) ^ "↦" ^ (to_string tr)
  ) |> String.concat "  \n  ") ^
  "\nObs:\n  " ^ (StateMap.bindings a.obs |> List.map ebinding_to_string  |>
  String.concat "  \n  ") ^ "\n\n"

let autom (e: Nk.t) : t =
  let add e m = match NkMap.find_opt e m with
                | Some n -> n, m
                | None -> let n = NkMap.cardinal m in
                          n, NkMap.add e n m in
  (* q: queue of states to visit
     visited: already processed states
     num: Nk ⇰ int numbering of states
     tr: transitions in progress
     ob: observations in progress *)
  let rec loop (q: Nk.t list) (visited: StateSet.t) (num: int NkMap.t) tr ob =
    match q with
    | [] -> {
      states = visited;
      start = 1;
      trans = tr;
      obs = ob;
    }
    | e0::rem -> if NkMap.mem e0 num then
                   loop rem visited num tr ob
                 else
                   let e0n, num' = add e0 num in
                   let vis = StateSet.add e0n visited in
                   let sts = Deriv.d e0 in
                   let ob' = StateMap.add e0n (Deriv.e e0) ob in
                   let q' = Sts.to_list sts |> List.map fst in
                   let num'' = List.fold_left (fun a e -> add e a |> snd) num' q' in
                   let ntr = Sts.to_list sts |> List.map (fun (e,spp) -> (NkMap.find e num'', spp))
                                             |> List.fold_left (fun a (s,spp) -> StateMap.add s spp a) StateMap.empty in
                   let tr' = StateMap.add e0n ntr tr in
                   loop (q'@rem) vis num'' tr' ob' in
    (* The order [e] and [drop] are added here establishes the invariant that
       the drop state is 0 and the start state is 1. *)
    let num0 = NkMap.empty in
    loop [Nk.drop; e] StateSet.empty num0 StateMap.empty StateMap.empty

let accept (a: t) (trace: Trace.t) : bool =
  let pairs = Trace.pairs trace in
  let rec acc state rem =
    match rem with
    | [] -> failwith "Unreachable"
    | [p] -> Spp.mem (StateMap.find state a.obs) p
    | p::rem' ->
        let sm = StateMap.find state a.trans in
        let state' = match List.find_map (fun (s,spp) -> if Spp.mem spp p then Some s else None) (StateMap.bindings sm) with
                     | None -> failwith ("No transition available for " ^ (Pkpair.to_string p))
                     | Some s -> s in
        acc state' rem' in
  acc a.start pairs

let rep (a: t) (fields: Field.S.t) : Trace.t =
  let rec backout (pk: Pk.t) (spps: Spp.t list) (partial: Trace.t) : Trace.t = 
    match spps with
    | [] -> partial
    | spp::rem -> 
        let pk' = Sp.rep (Spp.pull spp (Sp.of_pk pk)) fields in
        backout pk' rem (pk'::partial) in

  let rec r (q: (State.t * Sp.t * (Spp.t list)) list) (visited: Sp.t StateMap.t) =
    let state, sp, spps = try List.hd q
                          with _ -> failwith "Queue unexpectedly emptied" in

    let ob = StateMap.find state a.obs in
    let out = Spp.push sp ob in
    if not (Sp.eq out Sp.drop) then
      let pk = Spp.rep (Spp.seq_pair ob (Spp.of_sp out)) fields |> Pkpair.split |> snd in
      backout pk spps []
    else 
      let next = StateMap.find state a.trans |> StateMap.bindings in
      let unseen p = Sp.diff p (StateMap.find state visited) in
      let refine spp = Spp.seq_pair (Spp.of_sp sp) spp in
      let q' = List.map (fun (s, spp) -> s, unseen (Spp.push sp spp), (refine spp)::spps) next
               |> List.filter (fun (_, sp, _) -> not (Sp.eq sp Sp.drop)) in
      let v' = List.fold_left (fun a (s, sp, _) -> match StateMap.find_opt s a with
                                                  | None -> StateMap.add s sp a
                                                  | Some sp' -> StateMap.add s (Sp.union_pair sp sp') a) visited q' in
      r (q@q') v'
  in r [(a.start, Sp.skip, [])] (StateMap.singleton a.start Sp.skip)

(* states, start, trans, obs *)
let xor (a1: t) (a2: t) =
  let num: int PairMap.t = StateSet.fold (fun s1 m1 ->
                              StateSet.fold (fun s2 m2 -> 
                                PairMap.add (s1,s2) (PairMap.cardinal m2) m2
                              ) a2.states m1
                           ) a1.states PairMap.empty in
  let get s1 s2 = PairMap.find (s1,s2) num in
  let states = PairMap.bindings num |> List.map snd |> StateSet.of_list in
  let start = PairMap.find (a1.start, a2.start) num in
  let trans = StateMap.fold (fun s1s tr1 m1 ->
                StateMap.fold (fun s2s tr2 m2 ->
                  let m = StateMap.fold (fun s1t spp1 m3 ->
                    StateMap.fold (fun s2t spp2 m4 ->
                      let spp = Spp.intersect_pair spp1 spp2 in
                      StateMap.add (get s1t s2t) spp m4
                    ) tr1 m3
                  ) tr2 StateMap.empty  in
                  StateMap.add (get s1s s2s) m m2
                ) a2.trans m1)
              a1.trans StateMap.empty in
  let obs = StateMap.fold (fun s1 obs1 m1 ->
              StateMap.fold (fun s2 obs2 m2 ->
                StateMap.add (get s1 s2) (Spp.xor obs1 obs2) m2
              ) a2.obs m1
            ) a1.obs StateMap.empty in
  { states = states ; start = start ; trans = trans ; obs = obs }

let bisim (a1: t) (a2: t) : bool =
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem -> if State.eq s1 s2 || Sp.eq pk Sp.drop ||
                            (PairMap.mem (s1,s2) visited) && 
                            (Sp.le pk (PairMap.find (s1,s2) visited)) then
                           bq rem visited
                         else
                           let prev = match PairMap.find_opt (s1,s2) visited with
                                      | None -> Sp.drop
                                      | Some a -> a in
                           let rem = Sp.diff pk prev in
                         if not (Spp.eq (Spp.seq_pair (Spp.of_sp rem) (StateMap.find s1 a1.obs))
                                        (Spp.seq_pair (Spp.of_sp rem) (StateMap.find s2 a2.obs))) then
                           false
                         else
                           let tr1 = StateMap.find s1 a1.trans |> StateMap.bindings in
                           let tr2 = StateMap.find s2 a2.trans |> StateMap.bindings in
                           let next = List.fold_left (fun a (ei, sppi)->
                              (List.map (fun (ej, sppj) ->
                                let pk' = Spp.push rem (Spp.intersect_pair sppi sppj) in
                                (pk', ei, ej)) tr2)@a) [] tr1 in
                           let all1 = List.map (fun (_,spp) -> spp) tr1 |> Spp.union in
                           let all2  = List.map (fun (_,spp) -> spp) tr2 |> Spp.union in
                           let rem1 = List.map (fun (ei,sppi) ->
                               Spp.((push rem (diff sppi all2), ei, State.drop))) tr1 in
                           let rem2 = List.map (fun (ei,sppi) ->
                               Spp.((push rem (diff sppi all1), State.drop, ei))) tr2 in
                           let next' = next @ rem1 @ rem2 in

                           (* Update the visited set to include everything in
                              this packet (plus everything there already for this pair of states. *)
                           let vpk = Sp.union_pair prev rem in
                           let visited' = PairMap.add (s1,s2) vpk visited in
                           bq next' visited'
  in bq [(Sp.skip, a1.start, a2.start)] PairMap.empty

let forward (e: Nk.t) : Sp.t =
  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match NkMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in

  let rec loop (todo: (Nk.t * Sp.t) list) (visited: Sp.t NkMap.t) =
    match todo with
    | [] -> NkMap.bindings visited |>
            List.map (fun (e, pk) -> Spp.push pk (Deriv.e e)) |>
            Sp.union
    | (e, pkref) :: rem -> 
      let pk = !pkref in 
      match (e, pk) with 
      | (_, Sp.Drop) -> loop rem visited
      | (e, pk) ->
          if Nk.eq e Nk.drop then loop rem visited else
          let p = Sp.diff pkref (get visited e) in
          let v' = NkMap.add e (Sp.union_pair p (get visited e)) visited in
          let next = Deriv.d e
                    |> Sts.to_list
                    |> List.map (fun (e', spp) -> (e', Spp.push p spp)) in
          loop (next@rem) v'

  in loop [(e, Sp.skip)] NkMap.empty

let backward (e: Nk.t) : Sp.t = failwith "TODO: reimplement backward"
  (* Old implementation (assumes State = Nk)
  let a = autom e in

  let todo_init = 
    StateSet.elements a.states |> List.map (fun e -> (e, Spp.pull (Deriv.e e) Sp.skip)) in

  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match StateMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in

  let rec loop (todo: (State.t * Sp.t) list) (visited: Sp.t StateMap.t) =
    match todo with
    | [] -> get visited a.start
    | (e, pkref) :: rem -> 
      let pk = !pkref in 
      match (e, pk) with 
    | (_, Sp.Drop) -> loop rem visited
    | (e,pk) ->
        if State.eq e State.drop then loop rem visited else
        let p = Sp.diff pkref (get visited e) in
        let v' = StateMap.add e (Sp.union_pair p (get visited e)) visited in
        let next = StateSet.elements a.states 
                   |> List.map (fun e' -> (e', Deriv.d e' |> Sts.trans e))
                   |> List.map (fun (e', spp) -> (e', Spp.pull spp p)) in
        loop (next@rem) v'
  in loop todo_init StateMap.empty
  *)
