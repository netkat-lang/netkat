open Netkat_netkat

(* Ground-truth forward algorithm, transcribed directly and literally from
   Figure 8(i) of the KATch paper, operating on the explicit finite automaton
   (Nka.t) rather than on-the-fly over expressions. Uses genuine union-accumulate
   (+=) for both done(q) and todo(q), exactly as pseudocode specifies. *)
let paper_forward (a : Nka.t) : Sp.t =
  let open Nka in
  let done_ = Hashtbl.create 16 in
  let todo = Hashtbl.create 16 in
  let get tbl q = match Hashtbl.find_opt tbl q with None -> Sp.drop | Some x -> x in
  StateSet.iter (fun q -> Hashtbl.replace done_ q Sp.drop) a.states;
  StateSet.iter (fun q -> Hashtbl.replace todo q Sp.drop) a.states;
  Hashtbl.replace todo a.start Sp.skip;
  let trans_spp q q' =
    match StateMap.find_opt q a.trans with
    | None -> Spp.drop
    | Some m -> (match StateMap.find_opt q' m with None -> Spp.drop | Some s -> s)
  in
  let continue_ = ref true in
  while !continue_ do
    match StateSet.elements a.states |> List.find_opt (fun q -> not (Sp.eq (get todo q) Sp.drop)) with
    | None -> continue_ := false
    | Some q ->
      let p = Sp.diff (get todo q) (get done_ q) in
      Hashtbl.replace todo q Sp.drop;
      Hashtbl.replace done_ q (Sp.union_pair (get done_ q) p);
      StateSet.iter (fun q' ->
        let contribution = Spp.push p (trans_spp q q') in
        Hashtbl.replace todo q' (Sp.union_pair (get todo q') contribution)
      ) a.states
  done;
  StateSet.fold (fun q acc ->
    let obs = match StateMap.find_opt q a.obs with None -> Spp.drop | Some s -> s in
    Sp.union_pair acc (Spp.push (get done_ q) obs)
  ) a.states Sp.drop

(* Hypothesized fix for Nka.forward_init: identical structure, but todo_map
   entries are UNION-accumulated instead of overwritten with NkMap.add,
   matching the paper's todoR(q') += push(p, delta(q,q')). *)
module NkMap = Map.Make (Nk)

let fixed_forward_init (e : Nk.t) (init : Sp.t) : Sp.t =
  let get m exp = match NkMap.find_opt exp m with None -> Sp.drop | Some sp -> sp in
  let rec loop (todo : Nk.t list) (visited : Sp.t NkMap.t) (todo_map : Sp.t NkMap.t) =
    match todo with
    | [] ->
      NkMap.bindings visited
      |> List.map (fun (e, pk) -> Spp.push pk (Deriv.e e))
      |> Sp.union
    | e :: rem ->
      let pkref = get todo_map e in
      let pk = !pkref in
      (match (e, pk) with
       | _, Sp.Drop -> loop rem visited todo_map
       | e, _ ->
         if Nk.eq e Nk.drop then loop rem visited todo_map
         else
           let p = Sp.diff pkref (get visited e) in
           let v' = NkMap.add e (Sp.union_pair p (get visited e)) visited in
           let todo_map_reset = NkMap.add e Sp.drop todo_map in
           let next = Deriv.d e |> Sts.to_list in
           let next_states = List.map (fun (e, _) -> e) next in
           let next_todo_map =
             List.fold_left
               (fun m (e, spp) -> NkMap.add e (Sp.union_pair (get m e) (Spp.push p spp)) m)
               todo_map_reset next
           in
           loop (next_states @ rem) v' next_todo_map)
  in
  loop [ e ] NkMap.empty (NkMap.singleton e init)

(* Ground-truth backward algorithm, transcribed directly and literally from
   Figure 8(ii) of the KATch paper. Unlike forward, EVERY state is seeded
   up-front with pull(epsilon(q), final), not just the start state; this
   models the "zero-hop" case where a state's own output alone already
   matches. Uses genuine union-accumulate (+=) throughout, matching the
   paper's todoR(q') += pull(delta(q',q), p). *)
let paper_backward (a : Nka.t) (final : Sp.t) : Sp.t =
  let open Nka in
  let done_ = Hashtbl.create 16 in
  let todo = Hashtbl.create 16 in
  let get tbl q = match Hashtbl.find_opt tbl q with None -> Sp.drop | Some x -> x in
  let obs_spp q = match StateMap.find_opt q a.obs with None -> Spp.drop | Some s -> s in
  let trans_spp q q' =
    match StateMap.find_opt q a.trans with
    | None -> Spp.drop
    | Some m -> (match StateMap.find_opt q' m with None -> Spp.drop | Some s -> s)
  in
  StateSet.iter (fun q -> Hashtbl.replace done_ q Sp.drop) a.states;
  StateSet.iter (fun q -> Hashtbl.replace todo q (Spp.pull (obs_spp q) final)) a.states;
  let continue_ = ref true in
  while !continue_ do
    match StateSet.elements a.states |> List.find_opt (fun q -> not (Sp.eq (get todo q) Sp.drop)) with
    | None -> continue_ := false
    | Some q ->
      let p = Sp.diff (get todo q) (get done_ q) in
      Hashtbl.replace todo q Sp.drop;
      Hashtbl.replace done_ q (Sp.union_pair (get done_ q) p);
      StateSet.iter (fun q' ->
        (* q' -> q transition: contribute pull(delta(q',q), p) into todo(q') *)
        let contribution = Spp.pull (trans_spp q' q) p in
        Hashtbl.replace todo q' (Sp.union_pair (get todo q') contribution)
      ) a.states
  done;
  get done_ a.start

let fa = Field.get_or_assign_fid "a"
let fb = Field.get_or_assign_fid "b"
let fc = Field.get_or_assign_fid "c"
let fd = Field.get_or_assign_fid "d"
let v0 = Value.of_int 0
let v1 = Value.of_int 1
let v2 = Value.of_int 2
let v3 = Value.of_int 3

let check_expr label (e : Nk.t) =
  let r1 = Nka.forward e in
  let r2 = paper_forward (Nka.autom e) in
  let r3 = fixed_forward_init e Sp.skip in
  let ok12 = Sp.eq r1 r2 in
  let ok23 = Sp.eq r2 r3 in
  if not ok12 || not ok23 then begin
    Printf.printf "MISMATCH on %s\n" label;
    Printf.printf "  expr: %s\n" (Nk.to_string e);
    Printf.printf "  Nka.forward (current)      = %s\n" (Sp.to_string r1);
    Printf.printf "  paper_forward (ground truth)= %s\n" (Sp.to_string r2);
    Printf.printf "  fixed_forward_init          = %s\n" (Sp.to_string r3);
    Printf.printf "  current==ground truth? %b   fixed==ground truth? %b\n%!" ok12 ok23;
    true
  end else false

let check_backward_expr label (e : Nk.t) =
  let r1 = Nka.backward e in
  let r2 = paper_backward (Nka.autom e) Sp.skip in
  let ok = Sp.eq r1 r2 in
  if not ok then begin
    Printf.printf "BACKWARD MISMATCH on %s\n" label;
    Printf.printf "  expr: %s\n" (Nk.to_string e);
    Printf.printf "  Nka.backward (current)      = %s\n" (Sp.to_string r1);
    Printf.printf "  paper_backward (ground truth)= %s\n%!" (Sp.to_string r2);
    true
  end else false

(* Hand-crafted "diamond" expressions: A1 has one direct edge to a common
   target and one indirect (2-hop) edge to the SAME target, in several
   orderings/shapes to try to trigger unlucky worklist ordering. *)
let diamond1 () =
  (* a<-0 . dup . ( b<-1.dup + b<-2.dup.c<-1.dup ) *)
  Nk.seq
    [ Nk.modif fa v0; Nk.dup;
      Nk.union_pair
        (Nk.seq [ Nk.modif fb v1; Nk.dup ])
        (Nk.seq [ Nk.modif fb v2; Nk.dup; Nk.modif fc v1; Nk.dup ]) ]

let diamond2 () =
  (* swapped: b<-2.dup.c<-1.dup direct-looking branch listed first, with an extra dup-only tail on the "direct" branch so it isn't literally Nk.skip *)
  Nk.seq
    [ Nk.modif fa v0; Nk.dup;
      Nk.union_pair
        (Nk.seq [ Nk.modif fb v2; Nk.dup; Nk.modif fc v1; Nk.dup ])
        (Nk.seq [ Nk.modif fb v1; Nk.dup; Nk.modif fd v3; Nk.dup ]) ]

let diamond3 () =
  (* three-way convergence with different depths *)
  Nk.seq
    [ Nk.modif fa v0; Nk.dup;
      Nk.union
        [ Nk.seq [ Nk.modif fb v1; Nk.dup ];
          Nk.seq [ Nk.modif fb v2; Nk.dup; Nk.modif fc v1; Nk.dup ];
          Nk.seq [ Nk.modif fb v3; Nk.dup; Nk.modif fc v2; Nk.dup; Nk.modif fd v0; Nk.dup ] ] ]

let diamond_star () =
  (* introduce a cycle via star to force many re-visits / re-pushes to shared states *)
  Nk.star
    (Nk.union
       [ Nk.seq [ Nk.filter true fa v0; Nk.modif fa v1; Nk.dup ];
         Nk.seq [ Nk.filter true fa v1; Nk.modif fb v1; Nk.dup ];
         Nk.seq [ Nk.filter true fb v1; Nk.modif fa v0; Nk.modif fb v0; Nk.dup ] ])

let simple_probe () =
  (* dup.c=1 -- root's own zero-hop epsilon is bottom (dup unconsumed), but
     after one hop the state is just "c=1", which directly matches final.
     Correct backward answer: "c=1". *)
  Nk.seq [ Nk.dup; Nk.filter true fc v1 ]

let () =
  let any = ref false in
  Printf.printf "diamond1...\n%!";
  let m1 = check_expr "diamond1" (diamond1 ()) in
  Printf.printf "diamond2...\n%!";
  let m2 = check_expr "diamond2" (diamond2 ()) in
  Printf.printf "diamond3...\n%!";
  let m3 = check_expr "diamond3" (diamond3 ()) in
  any := !any || m1 || m2 || m3;
  Printf.printf "hand-crafted done, mismatch so far: %b\n%!" !any;

  Printf.printf "\n--- backward checks ---\n%!";
  let bany = ref false in
  Printf.printf "simple_probe (dup.c=1)...\n%!";
  let b0 = check_backward_expr "simple_probe" (simple_probe ()) in
  Printf.printf "diamond1...\n%!";
  let b1 = check_backward_expr "diamond1" (diamond1 ()) in
  Printf.printf "diamond2...\n%!";
  let b2 = check_backward_expr "diamond2" (diamond2 ()) in
  Printf.printf "diamond3...\n%!";
  let b3 = check_backward_expr "diamond3" (diamond3 ()) in
  bany := !bany || b0 || b1 || b2 || b3;
  Printf.printf "backward hand-crafted done, mismatch so far: %b\n%!" !bany;

  Random.self_init ();
  let fields = [ fa; fb; fc; fd ] in
  let values = [ v0; v1; v2; v3 ] in
  let n_trials = 2000 in
  for i = 1 to n_trials do
    let size = 2 + (i mod 6) in
    let e = Nk.rand fields values size in
    if i mod 200 = 0 then Printf.printf "  ...trial %d/%d\n%!" i n_trials;
    let m = check_expr (Printf.sprintf "random#%d (size %d): %s" i size (Nk.to_string e)) e in
    let mb = check_backward_expr (Printf.sprintf "backward-random#%d (size %d): %s" i size (Nk.to_string e)) e in
    any := !any || m;
    bany := !bany || mb
  done;
  if not !any then Printf.printf "FORWARD: No mismatches found across 3 hand-crafted + %d random expressions.\n" n_trials;
  if not !bany then Printf.printf "BACKWARD: No mismatches found across 4 hand-crafted + %d random expressions.\n" n_trials
