(** Some unit tests for the modules in Nerode. *)

open Nerode

(* More tests might be appropriate..*)

let w001 = Alphabet.w_of_ints [0;0;1]
let w10 = Alphabet.w_of_ints [1;0]

let rx001 = Rx.of_word w001
let rx10 = Rx.of_word w10

let rx001star = Rx.star rx001
let rx_empty = Rx.Empty

let dfa_empty = Dfa.of_rx (Alphabet.intalph 2) rx_empty
let dfa001 = Dfa.of_rx (Alphabet.intalph 2) rx001
let dfa10 = Dfa.of_rx (Alphabet.intalph 2) rx10

let nfa001 = Dfa.to_nfa dfa001
let nfa10 = Dfa.to_nfa dfa10

(* DFA tests *)
let dfa_is_empty () =
  Alcotest.(check bool) "same bool" true (Dfa.is_empty dfa_empty)

let dfa_not_empty () =
  Alcotest.(check bool) "same bool" false (Dfa.is_empty dfa001)

let dfa_rep () =
  Alcotest.(check bool) "same bool" true (Dfa.rep dfa001 = w001)

let dfa_accept () =
  Alcotest.(check bool) "same bool" true (Dfa.accept dfa001 w001)

let dfa_reject () =
  Alcotest.(check bool) "same bool" false (Dfa.accept dfa001 w10)

let union_accept_001 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.union dfa001 dfa10) w001)

let union_accept_10 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.union dfa001 dfa10) w10)

let sym_diff_empty () =
  Alcotest.(check bool) "same bool" false (Dfa.symdiff dfa001 dfa10 |> Dfa.is_empty)

let intersect_empty () =
  Alcotest.(check bool) "same bool" true (Dfa.intersect dfa001 dfa10 |> Dfa.is_empty)

let diff_accept_001 () =
  Alcotest.(check bool) "same bool" true (Dfa.accept (Dfa.diff dfa001 dfa10) w001)

let diff_reject_10 () =
  Alcotest.(check bool) "same bool" false (Dfa.accept (Dfa.diff dfa001 dfa10) w10)

let () =
  Alcotest.run "Dfa"
  [
    ( "empty",
      [
        Alcotest.test_case "empty" `Quick dfa_is_empty;
        Alcotest.test_case "not empty" `Quick dfa_not_empty;
      ]
    );
    ( "rep",
      [
        Alcotest.test_case "rep" `Quick dfa_rep;
      ]
    );
    ( "accept",
      [
        Alcotest.test_case "accept" `Quick dfa_accept;
        Alcotest.test_case "reject" `Quick dfa_reject;
      ]
    );
    ( "union",
      [
        Alcotest.test_case "accept 001" `Quick union_accept_001;
        Alcotest.test_case "accept 10" `Quick union_accept_10;

      ]
    );
    ( "sym-diff",
      [
        Alcotest.test_case "sym-diff empty" `Quick sym_diff_empty;
      ]
    );
    ( "intersect",
      [
        Alcotest.test_case "intersect empty" `Quick intersect_empty;
      ]
    );
    ( "diff",
      [
        Alcotest.test_case "diff accept 001" `Quick diff_accept_001;
        Alcotest.test_case "diff reject 10" `Quick diff_reject_10;
      ]
    );
  ]
  (*
  Alcotest.run "Nfa"
    [
      ( "empty",
        [
          Alcotest.test_case "empty" `Quick is_empty;
          Alcotest.test_case "not empty" `Quick not_empty;
        ] );
      ( "accept",
        [ 
          Alcotest.test_case "accept 11" `Quick reject_two_ones;
          Alcotest.test_case "accept 1010" `Quick reject_many_zeroes; 
          Alcotest.test_case "reject 111" `Quick reject_odd_ones;
          Alcotest.test_case "accept 000" `Quick accept_three_zeroes;
        ]);
      ( "union",
        [ 
          Alcotest.test_case "accept 00" `Quick union_accepts_even_zeroes;
          Alcotest.test_case "reject 10" `Quick 
            union_rejects_odd_ones_and_zeroes;
        ]);
      ( "concatenation",
        [ 
          Alcotest.test_case "accept 1100" `Quick 
            concatenation_accepts_even_ones_even_zeroes;
          Alcotest.test_case "reject 1110" `Quick 
            concatenation_rejects_odd_ones_odd_zeroes;
          Alcotest.test_case "accept 11100" `Quick 
            concatenation_accepts_odd_ones;
        ]);
      ( "kleene",
        [ 
          Alcotest.test_case "accept 000" `Quick kleene_accepts_three_zeroes;
          Alcotest.test_case "accept eps" `Quick kleene_accepts_empty;
          Alcotest.test_case "accept 000000" `Quick kleene_accepts_six_zeroes;
          Alcotest.test_case "accept 0000" `Quick kleene_rejects_four_zeroes;
        ]);
      ( "intersection",
        [ 
          Alcotest.test_case "accept 0101" `Quick intersection_accepts_even_ones_even_zeroes;
          Alcotest.test_case "reject 101" `Quick intersection_rejects_odd_zeroes;
          Alcotest.test_case "reject 010" `Quick intersection_rejects_odd_ones;
          Alcotest.test_case "reject 000" `Quick intersection_rejects_three_zeroes;
        ]);
      ( "epsilon remove",
        [ 
          Alcotest.test_case "accept 11" `Quick epsilon_remove_accepts_even_ones;
          Alcotest.test_case "reject 1" `Quick epsilon_remove_rejects_empty_string;
        ]);
    ]
*)
