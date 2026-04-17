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
      start = NkMap.find e num;
      trans = tr;
      obs = ob;
    }
    | e0::rem -> let e0n, num' = add e0 num in
                 if StateSet.mem e0n visited then
                   loop rem visited num tr ob
                 else
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
        match List.find_map (fun (s,spp) -> if Spp.mem spp p then Some s else None) (StateMap.bindings sm) with
        | None -> false
        | Some s -> acc s rem' in
  acc a.start pairs

  (** [rep a fields] computes a trace in the trace language of [a], using the
      packet fields in [fields]. The strategy is to peform a BFS, keeping a list
      of the SPPs taken as transitions along each path. When an output is reached,
      we ``backout'' by pulling the packet back through the SPPs. Note that the SPPs
      recorded also encode the sequence of symbolic packets which are actually
      observed along the path forward; this is necessary to ensure a misstep is
      not taken when backing out. *)
let rep (a: t) (fields: Field.S.t) : Trace.t =
  let rec backout (pk: Pk.t) (spps: Spp.t list) (partial: Trace.t) : Trace.t = 
    match spps with
    | [] -> partial
    | spp::rem -> 
        let pk' = Sp.rep (Spp.pull spp (Sp.of_pk pk)) fields in
        backout pk' rem (pk'::partial) in

  let rec r (q: (State.t * Sp.t * (Spp.t list)) list) (visited: Sp.t StateMap.t) =
    let state, sp, spps,qrem = match q with
                               | [] -> failwith "Queue unexpectedly emptied"
                               | (a,b,c)::d -> a,b,c,d in
    let ob = StateMap.find state a.obs in
    let out = Spp.push sp ob in
    if not (Sp.eq out Sp.drop) then
      let refined = Spp.seq_pair (Spp.of_sp sp) ob in
      let pk = Sp.rep out fields in
      backout pk (refined::spps) [pk]
    else 
      let unseen s p = match StateMap.find_opt s visited with
                       | None -> p
                       | Some p' -> Sp.diff p p' in
      let next = StateMap.find state a.trans |> StateMap.bindings in
      let refine spp = Spp.seq_pair (Spp.of_sp sp) spp in
      let q' = List.map (fun (s, spp) -> s, unseen s (Spp.push sp spp), (refine spp)::spps) next
               |> List.filter (fun (_, sp, _) -> not (Sp.eq sp Sp.drop)) in
      let v' = List.fold_left (fun a (s, sp, _) -> match StateMap.find_opt s a with
                                                   | None -> StateMap.add s sp a
                                                   | Some sp' -> StateMap.add s (Sp.union_pair sp sp') a) visited q' in
      r (qrem@q') v'
  in r [(a.start, Sp.skip, [])] (StateMap.singleton a.start Sp.skip)

(* This idea was fundamentally flawed...
let xor (a1: t) (a2: t) =
  let num: int PairMap.t = StateSet.fold (fun s1 m1 ->
                              StateSet.fold (fun s2 m2 -> 
                                (* Printf.printf "adding %d %d\n" s1 s2; *)
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
                      (* Printf.printf "getting %d %d\n" s1t s2t; *)
                      let spp = Spp.intersect_pair spp1 spp2 in
                      StateMap.add (get s1t s2t) spp m4
                    ) tr2 m3
                  ) tr1 StateMap.empty  in
                  StateMap.add (get s1s s2s) m m2
                ) a2.trans m1)
              a1.trans StateMap.empty in
  let obs = StateMap.fold (fun s1 obs1 m1 ->
              StateMap.fold (fun s2 obs2 m2 ->
                StateMap.add (get s1 s2) (Spp.xor obs1 obs2) m2
              ) a2.obs m1
            ) a1.obs StateMap.empty in
  { states = states ; start = start ; trans = trans ; obs = obs }
*)

(* Return true if the two automata are bisimilar and false otherwise. Note that
   because the automata are determnisitc by construction it is the case that
   they are bisimilar if and only if they are language equivalent.*)
let bisim (a1: t) (a2: t) : bool =
  let rec bq q visited = 
    match q with
    | [] -> true
    | (pk,s1,s2)::rem ->
        (* let () = Printf.printf "pk:%s state(%d,%d)\n%!" (Sp.to_string pk) s1 s2 in *)
        if Sp.eq pk Sp.drop ||
           (PairMap.mem (s1,s2) visited) && 
           (Sp.le pk (PairMap.find (s1,s2) visited)) then
          bq rem visited
        else
          let prev = match PairMap.find_opt (s1,s2) visited with
                     | None -> Sp.drop
                     | Some a -> a in
          let rem_pk = Sp.diff pk prev in
          let s1obs = StateMap.find s1 a1.obs in
          let s2obs = StateMap.find s2 a2.obs in
        if not (Spp.eq (Spp.seq_pair (Spp.of_sp rem_pk) s1obs)
                       (Spp.seq_pair (Spp.of_sp rem_pk) s2obs)) then
          (*
          let () = Printf.printf "pk:%s s1:%d s2:%d\n%!" (Sp.to_string rem) s1 s2 in
          let () = Printf.printf "obs1:%s obs2:%s\n%!" (Spp.to_string s1obs) (Spp.to_string s2obs) in
          *)
          false
        else
          let tr1 = StateMap.find s1 a1.trans |> StateMap.bindings in
          let tr2 = StateMap.find s2 a2.trans |> StateMap.bindings in
          let next = List.fold_left (fun a (ei, sppi)->
             (List.map (fun (ej, sppj) ->
               let pk' = Spp.push rem_pk (Spp.intersect_pair sppi sppj) in
               (pk', ei, ej)) tr2)@a) [] tr1 in
          let all1 = List.map (fun (_,spp) -> spp) tr1 |> Spp.union in
          let all2  = List.map (fun (_,spp) -> spp) tr2 |> Spp.union in
          let rem1 = List.map (fun (ei,sppi) ->
              Spp.((push rem_pk (diff sppi all2), ei, State.drop))) tr1 in
          let rem2 = List.map (fun (ei,sppi) ->
              Spp.((push rem_pk (diff sppi all1), State.drop, ei))) tr2 in
          let next' = next @ rem1 @ rem2 in

          (* Display next' *)
          (*
          let () = Printf.printf "from %d,%d\n" s1 s2 in
          let () = List.iter (fun (pk,t1,t2) -> Printf.printf "%s %d %d\n"
             (Sp.to_string pk) t1 t2) next' in
          *)

          (* Update the visited set to include everything in
             this packet (plus everything there already for this pair of states. *)
          let vpk = Sp.union_pair prev rem_pk in
          let visited' = PairMap.add (s1,s2) vpk visited in
          bq (next'@rem) visited'
  in bq [(Sp.skip, a1.start, a2.start)] PairMap.empty

(* Compute a traced in the symmetric difference between two automata. Return
   None if they are bisimilar. *)
let xor_rep (a1: t) (a2: t) (fields: Field.S.t) : (Value.Env.t * Trace.t option) list =
 Printf.printf "states A: %s\nstates B: %s\n" (to_string a1) (to_string a2);
  let rec backout (pk: Pk.t) (spps: Spp.t list) (partial: Trace.t) : Trace.t option = 
    match spps with
    | [] ->
        (* (*Debugging:*)
        if accept a1 partial = accept a2 partial then
          let () = Printf.printf "------a1------\n%s\n------a2------\n%s\n" (to_string a1) (to_string a2) in
          let () = Printf.printf "nonex: %s %s\n" (Trace.to_string partial) (string_of_bool @@ accept a1 partial) in
          failwith "Impossible: Trace identified not actually in symetric difference"
        else
          *)
          Some partial
    | spp::rem -> 
        let pk' = Sp.rep (Spp.pull spp (Sp.of_pk pk)) fields in
        backout pk' rem (pk'::partial) in
  let rec bq (en: Value.Env.t) (q: (Sp.t * Spp.t list * int * int) list) visited = (
    match q with
    | [] -> print_string "WIN\n"; [en,None]
    | (pk, spps, s1, s2)::rem ->
     let extend_env en binding = (
       let en2 = List.fold_left (fun en' (s,i) ->
         Value.Env.add s i en'
       ) en binding in
       en2
     ) in
    let result = (
       let print_collected () = (
         Hashtbl.iter (fun k v ->
           let s = Value.IntSet.fold (fun i acc -> Printf.sprintf "%s, %d" acc i) v "" in
           Printf.printf "  Collected: %s = %s\n" k s;
         ) Value.temp_assignments;
         let bindings = Value.get_temp_bindings () in
         List.iter (fun l ->
            let s = List.fold_left (fun acc (s,i) -> Printf.sprintf "%s, %s=%d" acc s i) "" l in
            Printf.printf "binding: %s\n" s;
         ) bindings
       ) in
       Printf.printf "\n\n### CHECK1 BEGIN\n";
       Value.start_collecting en;
       let check1 = Sp.eq pk Sp.drop ||
          (PairMap.mem (s1,s2) visited) && 
          (Sp.le pk (PairMap.find (s1,s2) visited)) in
       Value.collecting_assignments := false;
       Value.stop_collecting ();
       print_collected ();
       Printf.printf "\n\n### CHECK1 END\n";
       if check1 then
         (bq en rem visited)
       else (
         let prev = match PairMap.find_opt (s1,s2) visited with
                    | None -> Sp.drop
                    | Some a -> a in
         let rem_pk = Sp.diff pk prev in
         let s1obs = StateMap.find s1 a1.obs in
         let s2obs = StateMap.find s2 a2.obs in
         Printf.printf ">> states A: %d, states B: %d\n" (s1) (s2);
        (* check if these states produce the same observations *)
         let () = Printf.printf ">> obs1:%s obs2:%s\n{\n%!" (Spp.to_string (Spp.seq_pair (Spp.of_sp rem_pk) s1obs)) (Spp.to_string (Spp.seq_pair (Spp.of_sp rem_pk) s2obs)) in
         Printf.printf "\n\n### CHECK2 BEGIN\n";
         Value.start_collecting en;
         let seq1 = (Spp.seq_pair (Spp.of_sp rem_pk) s1obs) in
         Printf.printf "  ### CHECK A\n";
         let seq2 = (Spp.seq_pair (Spp.of_sp rem_pk) s2obs) in
         Printf.printf "  ### CHECK B\n";
         let check = not (Spp.eq2 seq1
                         seq2 true) in
         Value.stop_collecting ();
         print_collected ();
         Printf.printf "### CHECK2 END\n\n\n";
         if check then (
           Printf.printf "}\n";
           (*
           let () = Printf.printf "witness-difference:\n" in
           let () = Printf.printf "pk:%s s1:%d s2:%d\n%!" (Sp.to_string rem_pk) s1 s2 in
           let () = Printf.printf "obs1:%s obs2:%s\n%!" (Spp.to_string s1obs) (Spp.to_string s2obs) in
           *)
           let xorobs = Spp.xor s1obs s2obs in
           (* let () = Printf.printf "xorobs:%s\n%!" (Spp.to_string xorobs) in *)
           let out = Spp.push rem_pk xorobs in
           (* let () = Printf.printf "out:%s\n%!" (Sp.to_string out) in *)
           let last_spp = Spp.seq_pair (Spp.of_sp rem_pk) xorobs in
            Printf.printf ">> FAIL: BACKOUT\n";
           let out_rep = Sp.rep out fields in
           [en,backout out_rep (last_spp::spps) [out_rep]]
         ) else (
           Printf.printf "begin {\n";
           let tr1 = StateMap.find s1 a1.trans |> StateMap.bindings in
           let tr2 = StateMap.find s2 a2.trans |> StateMap.bindings in
           let next = List.fold_left (fun a (ei, sppi)->
              (List.map (fun (ej, sppj) ->
                let cap = Spp.intersect_pair sppi sppj in
                let pk' = Spp.push rem_pk cap in
                let spp = Spp.seq_pair (Spp.of_sp rem_pk) cap in
                (pk', spp::spps, ei, ej)) tr2)@a) [] tr1 in
           let all1 = List.map (fun (_,spp) -> spp) tr1 |> Spp.union in
           let all2  = List.map (fun (_,spp) -> spp) tr2 |> Spp.union in
           let rem1 = List.map (fun (ei,sppi) ->
               let diff = Spp.diff sppi all2 in
               let pk' = Spp.push rem_pk diff in
               let spp = Spp.seq_pair (Spp.of_sp rem_pk) diff in
               (pk', spp::spps, ei, State.drop)) tr1 in
           let rem2 = List.map (fun (ei,sppi) ->
               let diff = Spp.diff sppi all1 in
               let pk' = Spp.push rem_pk diff in
               let spp = Spp.seq_pair (Spp.of_sp rem_pk) diff in
               (pk', spp::spps, State.drop, ei)) tr2 in
           let next' = next @ rem1 @ rem2 in
           (*
           let () = Printf.printf "from %d,%d\n" s1 s2 in
           let () = List.iter (fun (pk,t1,t2) -> Printf.printf "%s %d %d\n"
              (Sp.to_string pk) t1 t2) next' in
           *)
           (* update visited set to include everything in this packet plus everything there already for this pair of states *)
           let vpk = Sp.union_pair prev rem_pk in
           let visited' = PairMap.add (s1,s2) vpk visited in
           Printf.printf "}\n";
           (bq en (next'@rem) visited')
         )
       )
    ) in
    let bindings = Value.get_temp_bindings () in
    result@
           List.fold_left (fun acc binding ->
             let en2 = extend_env en binding in
             if (Value.Env.compare en en2)<>0 then
             acc@(bq en2 q visited) else acc
           ) [] bindings
  ) in bq (*(Value.Env.add "y" 3 (Value.Env.singleton "x" 5))*)Value.Env.empty [(Sp.skip, [], a1.start, a2.start)] PairMap.empty

let forward_init (e: Nk.t) (init: Sp.t) : Sp.t =
  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match NkMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in
  let get_todo m exp = match NkMap.find_opt exp m with
                       | None -> Sp.drop
                       | Some sp -> sp in

  let rec loop (todo: Nk.t list) (visited: Sp.t NkMap.t) (todo_map : Sp.t NkMap.t) =
    match todo with
    | [] -> NkMap.bindings visited |>
            List.map (fun (e, pk) -> Spp.push pk (Deriv.e e)) |>
            Sp.union
    | e :: rem -> 
      let pkref = get_todo todo_map e in
      let pk = !pkref in 
      match (e, pk) with 
      | (_, Sp.Drop) -> loop rem visited todo_map
      | (e, pk) ->
          if Nk.eq e Nk.drop then loop rem visited todo_map else
          let p = Sp.diff pkref (get visited e) in
          let v' = NkMap.add e (Sp.union_pair p (get visited e)) visited in
          let next = Deriv.d e
                     |> Sts.to_list in
          let next_states = List.map (fun (e, _) -> e) next in
          let next_todo_map = List.fold_left (fun m (e, spp) -> NkMap.add e (Spp.push p spp) m) todo_map next in
          loop (next_states@rem) v' next_todo_map
  in loop [e] NkMap.empty (NkMap.singleton e init)

let forward (e: Nk.t) : Sp.t = forward_init e Sp.skip


let backward_final (e: Nk.t) (final : Sp.t) : Sp.t =
  let pull e = Spp.pull (Deriv.e e) final in
  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match NkMap.find_opt exp m with
                  | None -> Sp.drop
                  | Some sp -> sp in
  let get_todo m exp = match NkMap.find_opt exp m with
                       | None -> pull exp
                       | Some sp -> sp in

  let rec loop (todo: Nk.t list) (visited: Sp.t NkMap.t) (todo_map : Sp.t NkMap.t) =
    match todo with
    | [] -> get visited e
    | e :: rem -> 
      let pkref = get_todo todo_map e in
      let pk = !pkref in 
      match (e, pk) with 
      | (_, Sp.Drop) -> loop rem visited todo_map
      | (e, pk) ->
          if Nk.eq e Nk.drop then loop rem visited todo_map else
          let p = Sp.diff pkref (get visited e) in
          let v' = NkMap.add e (Sp.union_pair p (get visited e)) visited in
          let next = Deriv.d e
                     |> Sts.to_list in
          let next_states = List.map (fun (e, _) -> e) next in
          let next_todo_map = List.fold_left (fun m (e, spp) -> NkMap.add e (Spp.pull spp p) m) todo_map next in
          loop (next_states@rem) v' next_todo_map
  in loop [e] NkMap.empty NkMap.empty

let backward (e: Nk.t) : Sp.t = backward_final e Sp.skip

(*let backward (e: Nk.t) : Sp.t =
  let pull e = Spp.pull (Deriv.e e) Sp.skip in
  (* This definition of [get] has the effect that an exp missing
     from [visited] is equivalent to mapped to Drop *)
  let get m exp = match NkMap.find_opt exp m with
                  | None -> Sp.drop (*pull exp*)
                  | Some sp -> sp in

  let rec loop (todo: (Nk.t * Sp.t) list) (visited: Sp.t NkMap.t) =
    match todo with
    | [] -> (match NkMap.find_opt e visited with
      | None -> ref Sp.Drop
      | Some(sp) -> sp
      )
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
                     |> List.map (fun (e', spp) -> (e', Spp.pull spp p)) in
          loop (next@rem) v'
  in loop [(e, pull e)] NkMap.empty*)

  (* Old implementation (assumed State = Nk)
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

let size (t: t) : int * int = 
  let n = StateSet.cardinal t.states in
  let sum m = StateMap.fold (fun _ e -> (+) (Spp.size e)) m 0 in
  let obs = sum t.obs in
  let trans = StateMap.fold (fun _ m -> (+) (sum m)) t.trans 0 in
  n, obs + trans

let min (a1: t) (a2: t) =
  let n, m = size a1 in
  let n', m' = size a2 in
  if n < n' then
    a1
  else if n' < n then
    a2
  else if m < m' then
    a1
  else
    a2
