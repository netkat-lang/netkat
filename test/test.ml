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
      ]
    );
    ( "nka_backward_regressions",
      [
        Alcotest.test_case "dup" `Quick backward_regression_dup;
        Alcotest.test_case "dup_then_filter" `Quick backward_regression_dup_then_filter;
        Alcotest.test_case "union_unconditional_branch" `Quick backward_regression_union_unconditional_branch;
      ]
    );
  ]
