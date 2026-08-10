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
module PairSet = Set.Make(StatePair)
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

(* [List.length (accept_path a trace |> Option.get)] always equals
   [List.length (Trace.pairs trace)]: one entry per pair, the state that
   pair's own transition (trans, or obs for the trace's last pair) was
   evaluated from. Returns [None] if [trace] is rejected -- an interior
   pair has no matching [trans] edge, or the trace's last pair fails the
   [obs] of the state it's evaluated from. *)
let accept_path (a: t) (trace: Trace.t) : State.t list option =
  let pairs = Trace.pairs trace in
  let rec acc state rem =
    match rem with
    | [] -> failwith "Unreachable"
    | [p] -> if Spp.mem (StateMap.find state a.obs) p then Some [state] else None
    | p::rem' ->
        let sm = StateMap.find state a.trans in
        match List.find_map (fun (s,spp) -> if Spp.mem spp p then Some s else None) (StateMap.bindings sm) with
        | None -> None
        | Some s -> Option.map (fun rest -> state :: rest) (acc s rem') in
  acc a.start pairs

let accept (a: t) (trace: Trace.t) : bool =
  Option.is_some (accept_path a trace)

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

  (** Default cap on how many rounds [simulate_init] will unroll self-loops
      (e.g. the [net = (hop.dup)*] shape) looking for new witnesses, before
      giving up even if it hasn't reached a fixed point. This is a genuine
      safety net, not just a formality: a real cycle in the underlying
      topology graph (not merely a syntactic self-loop) could otherwise
      keep producing longer and longer distinct real paths forever. *)
  let default_max_rounds = 50

  (** How [simulate_init] should enumerate a given diversify field's
      values: [BestEffort] takes whatever [Sp.diversify_keys] finds
      locally at each state, which can under-enumerate a field whose live
      values are hidden behind a non-drop default or an as-yet-untested
      branch (there's no way to recover those from a single state's own
      output alone); [Exhaustive] instead enumerates against that field's
      full set of values anywhere reachable from [init] (computed once via
      [forward_over] and [Sp.collect_values], not per state); [Explicit]
      enumerates against a caller-supplied set instead of discovering one,
      for a field whose relevant values are known up front and cheaper to
      just state. *)
  type diversify_mode = BestEffort | Exhaustive | Explicit of Value.S.t

  (** [forward_over a init] is the union, over every state of [a], of the
      portion of that state's own output reachable from [init] -- the
      automaton-native analogue of [forward_init], computed directly from
      [a]'s own [states]/[trans]/[obs] rather than by re-deriving them
      from an [Nk.t] expression (which [simulate_init] doesn't have --
      only the already-built automaton). Unlike [simulate_init]'s own
      round-based exploration, this is an unconditional fixed point (no
      [max_rounds] needed): [a]'s state space is already finite, so the
      standard forward-dataflow worklist below is guaranteed to terminate,
      the same way [backward_final]'s does. *)
  let forward_over (a: t) (init: Sp.t) : Sp.t =
    let get m q = match StateMap.find_opt q m with None -> Sp.drop | Some sp -> sp in
    let rec loop (todo: State.t list) (done_: Sp.t StateMap.t) (todo_map: Sp.t StateMap.t) =
      match todo with
      | [] ->
          StateMap.bindings done_
          |> List.map (fun (q, sp) -> Spp.push sp (StateMap.find q a.obs))
          |> Sp.union
      | q :: rem ->
          let p = Sp.diff (get todo_map q) (get done_ q) in
          if Sp.eq p Sp.drop then loop rem done_ todo_map
          else
            let done_' = StateMap.add q (Sp.union_pair p (get done_ q)) done_ in
            let todo_map_reset = StateMap.add q Sp.drop todo_map in
            let next = StateMap.find q a.trans |> StateMap.bindings in
            let todo_map' = List.fold_left
              (fun m (s, spp) -> StateMap.add s (Sp.union_pair (get m s) (Spp.push p spp)) m)
              todo_map_reset next in
            loop (List.map fst next @ rem) done_' todo_map'
    in
    loop [a.start] StateMap.empty (StateMap.singleton a.start init)

  (* [destutter t] collapses consecutive duplicate packets in [t]. Backing
     out a path that passes through a state whose own output is the
     identity relation (as happens on every additional loop unroll of
     [net]'s trivial [epsilon(net) = top]) reproduces the same packet
     again with nothing new -- without collapsing that, "one more loop
     unroll" would look like "a new, longer trace" forever, and the
     round-based search below would never detect a fixed point. *)
  let destutter (t: Trace.t) : Trace.t =
    match t with
    | [] -> []
    | hd :: tl ->
      List.fold_left (fun acc pk -> match acc with
        | p :: _ when Pk.compare p pk = 0 -> acc
        | _ -> pk :: acc) [hd] tl
      |> List.rev

  (** [simulate_init ?max_rounds a init modes fields] explores states
      reachable from the symbolic input [init], unrolling self-loops round
      by round (rather than stopping after the first traversal, as a
      single reachability-bounded BFS would), and backs out one concrete
      trace per behaviorally-distinct branch of every visited state's own
      output -- rather than a single witness ([rep]) or the summarized
      final output set ([forward_init]).

      Exploration keeps a single merged [Sp.t] per automaton state (one
      frontier entry per reachable state, exactly as [forward_init]'s own
      bookkeeping does), not one per live diversify combination (i.e. per
      combination of values for a field with an entry in [modes]). What
      makes that sound is a "shadow field" tagged onto [init] once, up
      front, for every such field: for each field [f] with an entry in
      [modes], a fresh field [shadow_of f] (never referenced anywhere
      else) is set to match [f]'s value wherever [f] is live in [init],
      via [Sp.tag_origin]. [a]'s own transitions never touch [shadow_of
      f], so it rides along unmodified for the rest of exploration. That's
      what defeats the ambiguous-[Mod] bug: if two origins (e.g.
      [@dev=Medical_Device] and [@dev=Provider_Host]) later both hit a
      shared [Mod] that overwrites [dev], the composed relation
      [backout] inverts still has [shadow_of dev] as a live domain
      constraint, so inverting it against a specific witnessed packet has
      exactly one valid preimage, even though [backout] still just uses
      plain, unbranched [Sp.rep]. [shadow_of f] is stripped from every
      returned packet before it's ever seen outside this function.

      A field that only becomes live partway through [a] (e.g. a hub that
      dispatches on a second field) isn't shadow-tagged at [init], since
      there's nothing to tag before it's live -- but it's re-tagged the
      moment it *does* become live (every round, [retag] checks every
      diversify field for "live now, but its shadow isn't set yet" and
      tags exactly those), so it's protected against a many-to-one
      collapse of its own from that point on, the same as a field already
      live at [init]. Every state's own output is enumerated via
      [Sp.rep_over] over every field named in [modes] plus every shadow
      field, so genuine branches of those fields (not just
      shadow-distinguished origins) still contribute one trace per branch,
      while every other field picks a single arbitrary representative, as
      [rep] does.

      [modes] additionally says, per field, how thoroughly to enumerate
      it. [BestEffort] (the previous, only behavior) can under-enumerate a
      field whose live values are hidden behind a non-drop default or an
      untested branch at the specific state being examined -- there's
      nothing in that state's own output to recover them from.
      [Exhaustive] fields are instead enumerated against their full set of
      values anywhere reachable from [init], computed once up front (not
      per state) via [forward_over] and [Sp.collect_values] -- grouped
      into a single traversal for every [Exhaustive] field at once, rather
      than one per field. [Explicit vs] fields are enumerated against the
      caller-supplied [vs] instead of a computed domain. Both feed into
      [Sp.rep_over]/[Sp.tag_origin] via their own [domains] argument, so
      an [Exhaustive] or [Explicit] field is protected against under
      -enumeration at both the shadow-tagging and the final-witnessing
      step, not just one. *)
  let simulate_init ?(max_rounds = default_max_rounds) (a: t) (init: Sp.t) (modes: diversify_mode Field.M.t) (fields: Field.S.t) : Trace.t list =
  let diversify = Field.M.fold (fun f _ s -> Field.S.add f s) modes Field.S.empty in
  let exhaustive_fields =
    Field.M.fold (fun f m s -> match m with Exhaustive -> Field.S.add f s | _ -> s) modes Field.S.empty in
  let explicit_domains =
    Field.M.fold (fun f m acc -> match m with Explicit vs -> Field.M.add f vs acc | _ -> acc) modes Field.M.empty in
  let domains =
    if Field.S.is_empty exhaustive_fields then explicit_domains
    else Field.M.fold Field.M.add (Sp.collect_values exhaustive_fields (forward_over a init)) explicit_domains
  in
  let shadow_of =
    Field.S.fold (fun f m ->
      Field.M.add f (Field.get_or_assign_fid ("$origin$" ^ Field.get_or_fail_fid f)) m
    ) diversify Field.M.empty in
  let shadow_fields = Field.M.fold (fun _ f' s -> Field.S.add f' s) shadow_of Field.S.empty in
  let diversify_and_shadows = Field.S.union diversify shadow_fields in
  (* Tag whichever diversify fields are live now but not yet shadowed --
     called on [init] once, up front, and again on every round's [full_sp]
     to catch a field that's only just become live. *)
  let retag (sp: Sp.t) : Sp.t =
    let needing_tag = Field.M.filter (fun f f' -> Sp.is_tested f sp && not (Sp.is_tested f' sp)) shadow_of in
    if Field.M.is_empty needing_tag then sp else Sp.tag_origin ~domains needing_tag sp
  in
  let init' = retag init in
  let strip_shadows (t: Trace.t) : Trace.t =
    List.map (fun pk -> Field.S.fold Field.M.remove shadow_fields pk) t in
  (* When backing out through a hop where a diversify field [f] is live
     with more than one option (e.g. a self-loop that re-picks [f] every
     round, once its forward [Sp.t] has reached a fixed point), plain
     [Sp.rep] would tie-break arbitrarily -- independently of which
     shadow-tagged origin [pk] actually belongs to. That produces
     spurious, asymmetric detours: one origin's tie-break happens to
     agree with its own history, another's doesn't, so backing out
     "invents" an extra hop through a value the *other* origin never
     actually had, purely as an artifact of [Value]'s ordering. Since
     [shadow_of f] never changes once tagged, [pk]'s current value for it
     *is* this lineage's own history for [f]; sticking to it whenever
     it's still a live option (falling back to plain [Sp.rep] only when
     it isn't, e.g. [f] not tested at all yet) keeps every hop consistent
     with the origin already being reconstructed. *)
  let bias_to_origin (pk: Pk.t) (sp: Sp.t) : Sp.t =
    Field.M.fold (fun f f' acc ->
      match Field.M.find_opt f' pk with
      | None -> acc
      | Some v ->
          let narrowed = Sp.intersect_pair acc (Sp.of_pk (Field.M.singleton f v)) in
          if Sp.eq narrowed Sp.drop then acc else narrowed
    ) shadow_of sp in
  let rec backout (pk: Pk.t) (spps: Spp.t list) (partial: Trace.t) : Trace.t =
    match spps with
    | [] -> partial
    | spp::rem ->
        let preimage = bias_to_origin pk (Spp.pull spp (Sp.of_pk pk)) in
        let pk' = Sp.rep preimage fields in
        backout pk' rem (pk'::partial) in

  (* One round: for every active (state, reaching-set, path-so-far) entry,
     try to witness that state's own output, and compute the next round's
     frontier by following every outgoing edge one more hop. *)
  let round (frontier: (State.t * Sp.t * (Spp.t list)) list) (traces: Trace.S.t) =
    List.fold_left (fun (traces_acc, next_acc) (state, sp, spps) ->
      if Sp.eq sp Sp.drop then (traces_acc, next_acc) else
      let ob = StateMap.find state a.obs in
      let out = Spp.push sp ob in
      let traces_acc' =
        if Sp.eq out Sp.drop then traces_acc
        else
          let refined = Spp.seq_pair (Spp.of_sp sp) ob in
          let pks = Sp.rep_over ~domains diversify_and_shadows out fields in
          List.fold_left (fun acc pk ->
            Trace.S.add (strip_shadows (destutter (backout pk (refined::spps) [pk]))) acc
          ) traces_acc pks
      in
      let next = StateMap.find state a.trans |> StateMap.bindings in
      let next_acc' = List.fold_left (fun acc (s, spp) ->
        let full_sp = retag (Spp.push sp spp) in
        if Sp.eq full_sp Sp.drop then acc
        else (s, full_sp, (Spp.seq_pair (Spp.of_sp sp) spp) :: spps) :: acc
      ) next_acc next in
      (traces_acc', next_acc')
    ) (traces, []) frontier
  in
  (* Deliberately NOT stopping early just because one round found nothing
     new: a round can legitimately be "quiet" (e.g. an intermediate state
     whose own output is still bottom, dup not yet consumed) while later
     rounds still have real witnesses ahead -- picking a safe number of
     consecutive quiet rounds to tolerate before concluding "truly done"
     is guesswork that risks premature termination. [frontier = []] is
     unconditionally correct (nothing left to explore); [max_rounds] is
     the only other stopping condition, and is what actually bounds
     self-loops that never empty their own frontier. [Trace.S] dedup
     (after [destutter]) guarantees the final result is correct regardless
     of how many redundant rounds run past a real fixed point -- the only
     cost of not stopping early is some wasted work in that case. *)
  let rec go frontier rounds_left traces =
    if frontier = [] || rounds_left <= 0 then traces
    else
      let (traces', next_frontier) = round frontier traces in
      go next_frontier (rounds_left - 1) traces'
  in
  Trace.S.elements (go [(a.start, init', [])] max_rounds Trace.S.empty)

let simulate (a: t) (fields: Field.S.t) : Trace.t list = simulate_init a Sp.skip Field.M.empty fields

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
let xor_rep (a1: t) (a2: t) (fields: Field.S.t) : Trace.t option =
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
  let rec bq (q: (Sp.t * Spp.t list * int * int) list) visited = 
    match q with
    | [] -> None
    | (pk, spps, s1, s2)::rem ->
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
                           let () = Printf.printf "witness-difference:\n" in
                           let () = Printf.printf "pk:%s s1:%d s2:%d\n%!" (Sp.to_string rem_pk) s1 s2 in
                           let () = Printf.printf "obs1:%s obs2:%s\n%!" (Spp.to_string s1obs) (Spp.to_string s2obs) in
                           *)
                           let xorobs = Spp.xor s1obs s2obs in
                           (* let () = Printf.printf "xorobs:%s\n%!" (Spp.to_string xorobs) in *)
                           let out = Spp.push rem_pk xorobs in
                           (* let () = Printf.printf "out:%s\n%!" (Sp.to_string out) in *)
                           let last_spp = Spp.seq_pair (Spp.of_sp rem_pk) xorobs in
                           let out_rep = Sp.rep out fields in
                           backout out_rep (last_spp::spps) [out_rep]
                         else
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
                           (* Update the visited set to include everything in
                              this packet (plus everything there already for this pair of states. *)
                           let vpk = Sp.union_pair prev rem_pk in
                           let visited' = PairMap.add (s1,s2) vpk visited in
                           bq (next'@rem) visited'
  in bq [(Sp.skip, [], a1.start, a2.start)] PairMap.empty

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
          let todo_map_reset = NkMap.add e Sp.drop todo_map in
          let next = Deriv.d e
                     |> Sts.to_list in
          let next_states = List.map (fun (e, _) -> e) next in
          let next_todo_map = List.fold_left (fun m (e, spp) -> NkMap.add e (Sp.union_pair (get_todo m e) (Spp.push p spp)) m) todo_map_reset next in
          loop (next_states@rem) v' next_todo_map
  in loop [e] NkMap.empty (NkMap.singleton e init)

let forward (e: Nk.t) : Sp.t = forward_init e Sp.skip


(** Unlike [forward_init], this cannot be computed lazily state-by-state on the
    fly: propagating a state's contribution requires knowing all of its
    *predecessors*, which (unlike successors, given by [Deriv.d]) are not
    available without first knowing the full set of states. So we first
    materialize the automaton via [autom], invert its transition function once
    to get a predecessor map, then run the backward fixed point (Figure 8(ii)
    of [1]) directly over that explicit, finite automaton. *)
let backward_final (e: Nk.t) (final : Sp.t) : Sp.t =
  let a = autom e in
  let get m q = match StateMap.find_opt q m with
                | None -> Sp.drop
                | Some sp -> sp in
  let obs_spp q = match StateMap.find_opt q a.obs with
                  | None -> Spp.drop
                  | Some spp -> spp in
  (* preds.(q) is the list of (q', spp) such that q' --[spp]--> q. *)
  let preds = StateMap.fold (fun q' tr acc ->
    StateMap.fold (fun q spp acc ->
      StateMap.add q ((q', spp) :: (match StateMap.find_opt q acc with None -> [] | Some l -> l)) acc
    ) tr acc
  ) a.trans StateMap.empty in
  let get_preds q = match StateMap.find_opt q preds with None -> [] | Some l -> l in

  let rec loop (todo: State.t list) (done_: Sp.t StateMap.t) (todo_map : Sp.t StateMap.t) =
    match todo with
    | [] -> get done_ a.start
    | q :: rem ->
      let pkref = get todo_map q in
      let pk = !pkref in
      match pk with
      | Sp.Drop -> loop rem done_ todo_map
      | _ ->
          if State.eq q State.drop then loop rem done_ todo_map else
          let p = Sp.diff pkref (get done_ q) in
          let done_' = StateMap.add q (Sp.union_pair p (get done_ q)) done_ in
          let todo_map_reset = StateMap.add q Sp.drop todo_map in
          let preds_q = get_preds q in
          let pred_states = List.map fst preds_q in
          let todo_map' = List.fold_left
            (fun m (q', spp) -> StateMap.add q' (Sp.union_pair (get m q') (Spp.pull spp p)) m)
            todo_map_reset preds_q in
          loop (pred_states@rem) done_' todo_map'
  in
  let init_todo = StateSet.fold (fun q m -> StateMap.add q (Spp.pull (obs_spp q) final) m) a.states StateMap.empty in
  loop (StateSet.elements a.states) StateMap.empty init_todo

let backward (e: Nk.t) : Sp.t = backward_final e Sp.skip

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
