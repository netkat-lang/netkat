open Netkat_netkat

let stub_test () =
  Alcotest.(check bool) "same bool" true true

(* Regression tests for the Nka.forward algorithm (KATch paper, Figure 8(i)).

   These two expressions were found by differential fuzzing against an
   independent, literal transcription of the paper's pseudocode: the shipped
   forward_init accumulated pending contributions per-state by overwriting
   (NkMap.add) instead of union-accumulating (the paper's `todo(q') += ...`),
   which could silently drop contributions and yield an under- or
   over-approximate result. Both cases below were hand-verified by direct
   semantic reasoning about the NetKAT expressions (see PR/commit
   description), independent of any of the fixer/checker code. *)

let fa = Field.get_or_assign_fid "a"
let fb = Field.get_or_assign_fid "b"
let fc = Field.get_or_assign_fid "c"
let fd = Field.get_or_assign_fid "d"
let v0 = Value.of_int 0
let v1 = Value.of_int 1
let v2 = Value.of_int 2
let v3 = Value.of_int 3

(* [Nka.simulate_init] takes one [Nka.diversify_mode] per diversify field
   rather than a plain [Field.S.t]; this wraps a set into the all
   -[BestEffort] map most tests below just want (the previous behavior). *)
let best_effort fs = Field.S.fold (fun f m -> Field.M.add f Nka.BestEffort m) fs Field.M.empty

(* c<-3 + (c!=1 + dup*.b<-0).c=2.dup
   Hand-derivation: the c<-3 branch contributes "c=3" (b free); the other
   branch's two sub-cases are "c=2" (via c!=1, b free) and "c=2 & b=0" (via
   dup*.b<-0), and the latter is a subset of the former, so the whole
   expression's forward image is exactly "c=2 ∪ c=3" -- it must NOT depend on
   b. The buggy implementation returned "b≠0.c=3 ∪ b=0.(c=2 ∪ c=3))", which
   spuriously depends on b even though the c<-3 branch never touches b. *)
let forward_regression_diamond_union () =
  let e =
    Nk.union_pair (Nk.modif fc v3)
      (Nk.seq
         [ Nk.union_pair (Nk.filter false fc v1)
             (Nk.seq [ Nk.star Nk.dup; Nk.modif fb v0 ]);
           Nk.filter true fc v2;
           Nk.dup ])
  in
  Alcotest.(check string) "forward(c<-3 + (c!=1 + dup*.b<-0).c=2.dup) = c=2 ∪ c=3"
    "c=2 ∪ c=3" (Sp.to_string (Nka.forward e))

(* d<-0 + (b<-2 + dup*.c!=3).(dup + a=0)
   Hand-derivation: distributing the sequence over the trailing union gives
   four terms -- "b=2", "c!=3", "a=0 & b=2", "a=0 & c!=3" -- of which the last
   two are subsets of the first two, collapsing to "b=2 ∪ c!=3 ∪ d=0"
   (a is unconstrained throughout). In particular, the packet a=5,b=5,c=5,d=5
   must be a valid output (via the dup*.c!=3.dup path, which never touches
   a, b, or d). The buggy implementation's result excluded that packet
   whenever a != 0, which is unsound: it was dropping a reachable output. *)
let forward_regression_distribute_over_union () =
  let e =
    Nk.union_pair (Nk.modif fd v0)
      (Nk.seq
         [ Nk.union_pair (Nk.modif fb v2)
             (Nk.seq [ Nk.star Nk.dup; Nk.filter false fc v3 ]);
           Nk.union_pair Nk.dup (Nk.filter true fa v0) ])
  in
  Alcotest.(check string)
    "forward(d<-0 + (b<-2 + dup*.c!=3).(dup + a=0)) = b=2 ∪ b≠2.(c≠3 ∪ c=3.d=0)"
    "b=2 ∪ b≠2⋅(c≠3 ∪ c=3⋅d=0)" (Sp.to_string (Nka.forward e))

(* Nka.forward_over is an automaton-native reimplementation of the same
   fixed point Nka.forward_init computes over an Nk.t expression directly
   (needed because Nka.simulate_init only has the already-built automaton
   on hand, not the original expression) -- so it had better agree with
   forward_init on every expression, not just the trivial ones. Reuses
   both expressions above (one with a union-of-diamonds shape, one with a
   star and a distributed union) as a cross-check against the
   already-trusted forward_init, independent of any hand-derivation. *)
let forward_over_regression_matches_forward_init () =
  let check name e =
    let a = Nka.autom e in
    Alcotest.(check bool) (name ^ ": forward_over a init = forward_init e init")
      true (Sp.eq (Nka.forward_over a Sp.skip) (Nka.forward_init e Sp.skip)) in
  check "diamond_union"
    (Nk.union_pair (Nk.modif fc v3)
      (Nk.seq
         [ Nk.union_pair (Nk.filter false fc v1)
             (Nk.seq [ Nk.star Nk.dup; Nk.modif fb v0 ]);
           Nk.filter true fc v2;
           Nk.dup ]));
  check "distribute_over_union"
    (Nk.union_pair (Nk.modif fd v0)
      (Nk.seq
         [ Nk.union_pair (Nk.modif fb v2)
             (Nk.seq [ Nk.star Nk.dup; Nk.filter false fc v3 ]);
           Nk.union_pair Nk.dup (Nk.filter true fa v0) ]))

(* Regression tests for the Nka.backward algorithm (KATch paper, Figure 8(ii)).

   Backward has a fundamentally different structure than forward: propagating
   a state's newly-discovered contribution requires knowing its
   *predecessors* (states with an edge into it), not its successors. The
   shipped backward_final tried to reuse forward_init's on-the-fly,
   discover-states-as-you-go style, which only ever has a state's successors
   (via Deriv.d) on hand -- so it propagated each state's contribution onto
   its successors instead of its predecessors, which is simply a different
   (and wrong) computation. In particular, any state whose own zero-hop
   output is bottom (e.g. any expression with an unconsumed leading dup)
   immediately short-circuited the whole computation to "drop", instead of
   exploring further transitions -- so backward was badly broken for
   essentially any realistic dup-containing program. All three cases below
   were hand-verified by direct semantic reasoning about the NetKAT
   expressions (see PR/commit description), independent of the fix. *)

(* dup
   Hand-derivation: dup's own zero-hop output is bottom (must take at least
   one hop), but after that one hop (packet unchanged) we reach the terminal
   "skip" state, whose output is the identity relation, matching any packet.
   So every input packet eventually produces some output: backward(dup) must
   be "skip" (all packets). The buggy implementation returned "drop" because
   it treated dup's own empty zero-hop seed as "nothing to explore here" and
   never looked at dup's transition at all. *)
let backward_regression_dup () =
  let e = Nk.dup in
  Alcotest.(check string) "backward(dup) = skip"
    "skip" (Sp.to_string (Nka.backward e))

(* dup.c=1
   Hand-derivation: same as above, but after the one hop we reach the
   terminal state "c=1" (a bare filter), whose own output is exactly the
   packets with c=1. So backward(dup.c=1) = "c=1". The buggy implementation
   again returned "drop", for the same short-circuit reason. *)
let backward_regression_dup_then_filter () =
  let e = Nk.seq [ Nk.dup; Nk.filter true fc v1 ] in
  Alcotest.(check string) "backward(dup.c=1) = c=1"
    "c=1" (Sp.to_string (Nka.backward e))

(* (a=0 + c<-0).dup
   Hand-derivation: the c<-0 branch is an unconditional mod with no test, so
   for ANY input packet it reaches the trailing dup and then the terminal
   "skip" state (output = identity, matches anything). Since this branch
   alone accepts every input unconditionally, the whole expression's
   backward-relevant set is everything: "skip". The buggy implementation
   returned "drop", since the whole expression's own zero-hop epsilon is
   bottom (trailing, unconsumed dup) regardless of the union underneath it. *)
let backward_regression_union_unconditional_branch () =
  let e = Nk.seq [ Nk.union_pair (Nk.filter true fa v0) (Nk.modif fc v0); Nk.dup ] in
  Alcotest.(check string) "backward((a=0 + c<-0).dup) = skip"
    "skip" (Sp.to_string (Nka.backward e))

(* Tests for Nka.simulate_init: it explores states reachable from the given
   symbolic input, and returns one representative concrete trace per
   distinct automaton state with non-empty output -- i.e. it exercises every
   state, not every value-combination within a single relation. The number
   of traces returned is therefore bounded by the (finite) number of
   automaton states, not by how many rule-branches happen to feed into any
   one of them. *)

let fsw = Field.get_or_assign_fid "sw"
let fpt = Field.get_or_assign_fid "pt"

(* sw=1.dup.pt<-10 + sw=2.dup.pt<-20.dup.pt<-99
   Two branches of different depth reach two DIFFERENT non-empty-output
   states (residual "pt<-10" after 1 hop, residual "pt<-99" after 2 hops),
   so simulate must return exactly 2 traces, one per state. Hand-verified:
   the sw=1 branch's witness is [sw=1,pt=0];[sw=1,pt=10] (packet unchanged
   across the one dup -- collapsed by destutter -- then pt<-10 applied as
   the final, dup-free tail); the sw=2 branch's witness is
   [sw=2,pt=0];[sw=2,pt=20];[sw=2,pt=99] (pt<-20 applied before the second
   dup, pt<-99 applied as the final tail). *)
let simulate_regression_two_states () =
  let e =
    Nk.union_pair
      (Nk.seq [ Nk.filter true fsw v1; Nk.dup; Nk.modif fpt (Value.of_int 10) ])
      (Nk.seq
         [ Nk.filter true fsw v2; Nk.dup; Nk.modif fpt (Value.of_int 20);
           Nk.dup; Nk.modif fpt (Value.of_int 99) ])
  in
  let a = Nka.autom e in
  let traces = Nka.simulate_init a Sp.skip (best_effort Field.S.empty) (Field.get_fields ()) in
  let strs = List.map Trace.to_string traces |> List.sort compare in
  (* Other fields (a,b,c,d) registered by earlier tests in this same process
     are also picked up by Field.get_fields () and filled in via
     Value.choose; only sw/pt are actually constrained by this expression. *)
  Alcotest.(check (list string)) "simulate finds one trace per distinct state, both branches"
    (List.sort compare
       [ "[a=0,b=0,c=0,d=0,sw=1,pt=0];[a=0,b=0,c=0,d=0,sw=1,pt=10]";
         "[a=0,b=0,c=0,d=0,sw=2,pt=0];[a=0,b=0,c=0,d=0,sw=2,pt=20];[a=0,b=0,c=0,d=0,sw=2,pt=99]" ])
    strs

(* a<-1 + a<-2 + a<-3
   All three branches are unconditional mods with no test, so they all have
   overlapping domains; Sts.add merges any transitions sharing a target
   state, so this automaton has exactly one non-trivial state (the shared
   terminal residual), regardless of how many rule-branches feed into it.
   simulate must therefore return exactly 1 trace here, not 3 -- pinning
   down that simulate exercises automaton states, not every value
   enumerable from a single (possibly multi-valued) transition relation. *)
let simulate_regression_merged_state_gives_one_trace () =
  let e = Nk.union [ Nk.modif fa v1; Nk.modif fa v2; Nk.modif fa v3 ] in
  let a = Nka.autom e in
  let traces = Nka.simulate_init a Sp.skip (best_effort Field.S.empty) (Field.get_fields ()) in
  Alcotest.(check int) "simulate on a<-1+a<-2+a<-3 gives exactly 1 trace (one merged state)"
    1 (List.length traces)

(* Same a<-1 + a<-2 + a<-3 automaton as above, but with field [a] passed as
   a diversify set: since the single merged self-loop's own output relation
   branches on [a] (the three mods have overlapping domains and share a
   target, so Sts.add unions them into one multi-valued transition), asking
   to diversify over [a] must enumerate all three branches -- one trace per
   value -- rather than the single arbitrary representative from
   [simulate_regression_merged_state_gives_one_trace]. This is the fix for
   the case where the real behavioral diversity of a *(hop.dup)⋆-style
   network policy lives entirely inside one collapsed relation, not across
   distinct automaton states. *)
let simulate_regression_diversify_enumerates_merged_branches () =
  let e = Nk.union [ Nk.modif fa v1; Nk.modif fa v2; Nk.modif fa v3 ] in
  let a = Nka.autom e in
  let traces = Nka.simulate_init a Sp.skip (best_effort (Field.S.singleton fa)) (Field.get_fields ()) in
  let strs = List.map Trace.to_string traces |> List.sort compare in
  (* Other fields registered by earlier tests in this same process (b,c,d,
     and sw,pt from simulate_regression_two_states) are also picked up by
     Field.get_fields () and filled in via Value.choose; only a is actually
     diversified/constrained by this expression. *)
  Alcotest.(check (list string)) "simulate with diversify={a} enumerates all 3 branches of the merged relation"
    (List.sort compare
       [ "[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=1,b=0,c=0,d=0,sw=0,pt=0]";
         "[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=2,b=0,c=0,d=0,sw=0,pt=0]";
         "[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=3,b=0,c=0,d=0,sw=0,pt=0]" ])
    strs

(* ((a<-1 + a<-2).dup)*, diversify={a}
   Mirrors the shape of a real (hop.dup)* network policy at small scale:
   the automaton has exactly one real, self-looping state, whose own
   epsilon is always top (zero iterations is always valid), and whose
   self-loop edge carries the real, a-branching behavior. This pins down
   that simulate_init unrolls the self-loop round by round (finding the
   genuine 2-hop path "a=0;a=1;a=2", not just the two 1-hop witnesses),
   and that max_rounds genuinely bounds how many rounds it unrolls:
   max_rounds=1 only ever sees the trivial zero-hop witness; max_rounds=2
   additionally sees both 1-hop witnesses (but not yet the 2-hop one,
   which needs the self-loop traversed twice); the default cap (50) finds
   all 5 (the zero-hop witness, both 1-hop witnesses, and both orderings
   of the 2-hop witness). Further rounds add nothing new: from round 2 on,
   the self-loop's own forward [Sp.t] has reached a fixed point where both
   values of [a] are simultaneously live, so backing out through any
   number of additional rounds keeps reconstructing a history consistent
   with whichever origin the witness being backed out belongs to (see
   [simulate_init]'s [bias_to_origin]), and duplicate, longer
   reconstructions of an already-found trace just destutter back down to
   it rather than growing into a genuinely new, longer one. Since
   exploration keeps a single merged [Sp.t] per automaton state (no
   per-diversify-combination frontier splitting), the frontier here never
   grows past one entry regardless of max_rounds -- unlike a design that
   split per combination, which would double its frontier every round on
   this exact shape. *)
let simulate_regression_max_rounds_bounds_self_loop_unrolling () =
  let e = Nk.star (Nk.seq [ Nk.union_pair (Nk.modif fa v1) (Nk.modif fa v2); Nk.dup ]) in
  let a = Nka.autom e in
  let count mr = List.length (Nka.simulate_init ~max_rounds:mr a Sp.skip (best_effort (Field.S.singleton fa)) (Field.get_fields ())) in
  Alcotest.(check int) "max_rounds=1 only finds the trivial zero-hop witness" 1 (count 1);
  Alcotest.(check int) "max_rounds=2 additionally finds both 1-hop witnesses" 3 (count 2);
  Alcotest.(check int) "default max_rounds finds both orderings of the 2-hop witness too" 5 (count 50)

(* [@a=1 + @a=3] simulated on (dup.a<-0.dup), diversify={a}
   Direct regression test for the ambiguous-shared-Mod bug: two distinct
   diversify origins (a=1, a=3) both flow through an unconditional a<-0
   Mod that erases the very field distinguishing them, with ANOTHER dup
   after the Mod -- so witnessing happens one round later than the Mod
   itself, not immediately at the Mod's own state. The buggy
   implementation inverted a<-0 via a single, unbranched Sp.rep (or, in an
   intermediate attempt, deduplicated frontier entries that reconverged on
   the same (state, sp) one round before either was due to be witnessed),
   either way deterministically dropping one origin's trace. Correct
   behavior finds both. *)
let simulate_regression_diverse_origins_survive_shared_mod () =
  let init = Sp.union_pair (Sp.of_pk (Field.M.singleton fa v1)) (Sp.of_pk (Field.M.singleton fa v3)) in
  let e = Nk.seq [ Nk.dup; Nk.modif fa v0; Nk.dup ] in
  let a = Nka.autom e in
  let traces = Nka.simulate_init a init (best_effort (Field.S.singleton fa)) (Field.get_fields ()) in
  let strs = List.map Trace.to_string traces |> List.sort compare in
  Alcotest.(check (list string)) "both origins survive the shared a<-0 Mod, one round later"
    (List.sort compare
       [ "[a=1,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0]";
         "[a=3,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0]" ])
    strs

(* [@a=1 + @a=3] simulated on (dup.a<-0.dup.(b<-2+b<-3)), diversify={a,b}
   Direct regression test for the untested-diversify-field bug: [b] is a
   diversify field but is never tested anywhere near the origin -- it only
   becomes live several hops later (mirroring a hub-dispatch-style policy
   that branches on a second field partway through). The buggy
   Sp.restrict_over (built by reusing Sp.rep_over and filtering) filled in
   [b] with an arbitrary Value.choose default the moment it split on [a],
   permanently excluding every [b] value but that one from the rest of the
   exploration. Correct behavior finds all four (origin x branch)
   combinations, not just whichever [b] default got picked early. *)
let simulate_regression_untested_diversify_field_stays_free () =
  let init = Sp.union_pair (Sp.of_pk (Field.M.singleton fa v1)) (Sp.of_pk (Field.M.singleton fa v3)) in
  let e = Nk.seq [ Nk.dup; Nk.modif fa v0; Nk.dup; Nk.union_pair (Nk.modif fb v2) (Nk.modif fb v3) ] in
  let a = Nka.autom e in
  let traces = Nka.simulate_init a init (best_effort (Field.S.of_list [fa; fb])) (Field.get_fields ()) in
  let strs = List.map Trace.to_string traces |> List.sort compare in
  Alcotest.(check (list string)) "all four (origin x b-branch) combinations survive"
    (List.sort compare
       [ "[a=1,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=2,c=0,d=0,sw=0,pt=0]";
         "[a=1,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=3,c=0,d=0,sw=0,pt=0]";
         "[a=3,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=2,c=0,d=0,sw=0,pt=0]";
         "[a=3,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=0,c=0,d=0,sw=0,pt=0];[a=0,b=3,c=0,d=0,sw=0,pt=0]" ])
    strs

(* (b=2.c<-10 + b!=2), diversify={b}
   Direct regression test for the wildcard-collapsing bug in
   Sp.diversify_keys: this automaton has exactly one state, and its own
   output tests [b] against exactly one explicit value (2, the branch that
   also sets c<-10) with a non-drop default covering everything else
   (b<>2, identity) -- there is nothing in that state's own output to
   recover which OTHER values of [b] the default region actually covers.
   Best-effort diversify_keys tags the whole default region with one
   arbitrary value (Value.val_outside {2} = 0), so plain diversify={b}
   finds only 2 of however many values of [b] actually reach this state
   (2 and 0) -- 1 and 3 are silently dropped. Passing
   [Nka.Explicit {0;1;2;3}] for [b] instead forces every value in that
   declared domain not already explicit to get its own copy of the
   default region's continuation, recovering all four. *)
let simulate_regression_explicit_domain_recovers_default_values () =
  let e = Nk.union_pair
      (Nk.seq [ Nk.filter true fb v2; Nk.modif fc (Value.of_int 10) ])
      (Nk.filter false fb v2) in
  let a = Nka.autom e in
  let b_values traces =
    List.map (fun tr -> Field.M.find fb (List.hd tr) |> Value.to_int) traces
    |> List.sort_uniq Int.compare in
  let best_effort_traces = Nka.simulate_init a Sp.skip (best_effort (Field.S.singleton fb)) (Field.get_fields ()) in
  Alcotest.(check (list int)) "best-effort finds only the explicit branch and the default's one arbitrary tag"
    [0; 2] (b_values best_effort_traces);
  let explicit = Field.M.singleton fb (Nka.Explicit (Value.S.of_list [ v0; v1; v2; v3 ])) in
  let explicit_traces = Nka.simulate_init a Sp.skip explicit (Field.get_fields ()) in
  Alcotest.(check (list int)) "Explicit {0,1,2,3} recovers all four values hidden behind the default"
    [0; 1; 2; 3] (b_values explicit_traces)

(* sw=1.(b=2.c<-10 + b!=2).dup + sw=2.(b=0 + b=1 + b=3).dup.d<-9, diversify={b}
   Companion to the [Explicit] test above, for [Exhaustive]: the same
   wildcard-collapsing shape reaches one state (gated behind, and so
   distinguished by, sw=1), but this time [b]'s other values (0, 1, 3) are
   never given to simulate_init directly -- they only become explicit on
   a completely separate branch gated behind sw=2, reaching a different
   automaton state, mirroring a real network where a field's fuller
   domain becomes explicit somewhere else in the topology, not in a
   caller-supplied set. The sw=1/sw=2 gating matters: without it, the two
   branches' overlapping domains on [b] (sw=1's "b<>2" default and sw=2's
   "b in {0,1,3}" both cover b=0,1,3) make Sts.add -- which keeps a
   state's outgoing transitions disjoint by construction -- merge them
   into one combined target for that overlap, which would incidentally
   make [b] explicit there even under BestEffort and defeat the test.
   Gating on the otherwise-unused field [sw] keeps the two branches'
   domains disjoint, so they reach genuinely separate states, and [b]'s
   own local under-enumeration at the sw=1 state survives untouched.
   [Exhaustive] must recover the full domain via
   [forward_over]/[Sp.collect_values] over the whole automaton, not by
   being told. *)
let simulate_regression_exhaustive_domain_recovers_default_values () =
  let e = Nk.union_pair
      (Nk.seq [
         Nk.filter true fsw v1;
         Nk.union_pair
           (Nk.seq [ Nk.filter true fb v2; Nk.modif fc (Value.of_int 10) ])
           (Nk.filter false fb v2);
         Nk.dup ])
      (Nk.seq [
         Nk.filter true fsw v2;
         Nk.union [ Nk.filter true fb v0; Nk.filter true fb v1; Nk.filter true fb v3 ];
         Nk.dup;
         Nk.modif fd (Value.of_int 9) ])
  in
  let a = Nka.autom e in
  (* Only the sw=1 branch is the one under test -- the sw=2 branch already
     has b fully explicit on its own and would make any b-value trivially
     findable regardless of BestEffort/Exhaustive. sw itself is never
     modified anywhere, so it reliably distinguishes the two branches at
     every packet in a trace, unlike a field one of the branches modifies
     partway through (backing out through a Mod makes its prior value
     arbitrary again, as established earlier this session). *)
  let b_values_at_sw1 traces =
    List.filter (fun tr -> Field.M.find fsw (List.hd tr) |> Value.to_int = 1) traces
    |> List.map (fun tr -> Field.M.find fb (List.hd tr) |> Value.to_int)
    |> List.sort_uniq Int.compare in
  let best_effort_traces = Nka.simulate_init a Sp.skip (best_effort (Field.S.singleton fb)) (Field.get_fields ()) in
  Alcotest.(check (list int)) "best-effort still under-enumerates the sw=1 branch on its own"
    [0; 2] (b_values_at_sw1 best_effort_traces);
  let exhaustive = Field.M.singleton fb Nka.Exhaustive in
  let exhaustive_traces = Nka.simulate_init a Sp.skip exhaustive (Field.get_fields ()) in
  Alcotest.(check (list int)) "Exhaustive recovers all four values via the separate sw=2 branch"
    [0; 1; 2; 3] (b_values_at_sw1 exhaustive_traces)

let () =
  Alcotest.run "Stub"
  [
    ( "stub",
      [
        Alcotest.test_case "stub" `Quick stub_test;
      ]
    );
    ( "nka_forward_regressions",
      [
        Alcotest.test_case "diamond_union" `Quick forward_regression_diamond_union;
        Alcotest.test_case "distribute_over_union" `Quick forward_regression_distribute_over_union;
        Alcotest.test_case "forward_over_matches_forward_init" `Quick forward_over_regression_matches_forward_init;
      ]
    );
    ( "nka_backward_regressions",
      [
        Alcotest.test_case "dup" `Quick backward_regression_dup;
        Alcotest.test_case "dup_then_filter" `Quick backward_regression_dup_then_filter;
        Alcotest.test_case "union_unconditional_branch" `Quick backward_regression_union_unconditional_branch;
      ]
    );
    ( "nka_simulate_tests",
      [
        Alcotest.test_case "two_states" `Quick simulate_regression_two_states;
        Alcotest.test_case "merged_state_gives_one_trace" `Quick simulate_regression_merged_state_gives_one_trace;
        Alcotest.test_case "diversify_enumerates_merged_branches" `Quick simulate_regression_diversify_enumerates_merged_branches;
        Alcotest.test_case "max_rounds_bounds_self_loop_unrolling" `Quick simulate_regression_max_rounds_bounds_self_loop_unrolling;
        Alcotest.test_case "diverse_origins_survive_shared_mod" `Quick simulate_regression_diverse_origins_survive_shared_mod;
        Alcotest.test_case "untested_diversify_field_stays_free" `Quick simulate_regression_untested_diversify_field_stays_free;
        Alcotest.test_case "explicit_domain_recovers_default_values" `Quick simulate_regression_explicit_domain_recovers_default_values;
        Alcotest.test_case "exhaustive_domain_recovers_default_values" `Quick simulate_regression_exhaustive_domain_recovers_default_values;
      ]
    );
  ]
