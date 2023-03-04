(* open Alcotest *)
open Nerode
open Nfa
open Dfa

Nfa.

let is_empty () =
  Alcotest.(check bool) "same bool" true (IntNfa.empty int_nfa_empty)

let not_empty () =
  Alcotest.(check bool) "same bool" false (IntNfa.empty int_nfa_1)

let reject_two_ones () =
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept int_nfa_1 [Some 1; Some 1])

let reject_odd_ones () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept int_nfa_1 [Some 1; Some 1; Some 1])

let reject_many_zeroes () =
  Alcotest.(check bool) "same bool" true
    (IntNfa.accept int_nfa_1 [Some 1; Some 0; Some 1; Some 0])

let accept_three_zeroes () = 
  Alcotest.(check bool) "same bool" true
    (IntNfa.accept int_nfa_3 [Some 0; Some 0; Some 0])

let union_accepts_even_zeroes () =
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.union int_nfa_1 int_nfa_2) [Some 0; Some 0])

let union_rejects_odd_ones_and_zeroes () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.union int_nfa_1 int_nfa_2) [Some 1; Some 0])

let concatenation_accepts_even_ones_even_zeroes () = 
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.concatenation int_nfa_1 int_nfa_2) 
       [Some 1; Some 1; Some 0; Some 0])

let concatenation_rejects_odd_ones_odd_zeroes () = 
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.concatenation int_nfa_1 int_nfa_2) 
       [Some 1; Some 1; Some 1; Some 0;])

let concatenation_accepts_odd_ones () = 
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.concatenation int_nfa_1 int_nfa_2) 
       [Some 1; Some 1; Some 1; Some 0; Some 0])

let kleene_accepts_empty () = 
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.kleene int_nfa_3) [])

let kleene_accepts_three_zeroes () = 
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.kleene int_nfa_3) [Some 0; Some 0; Some 0])

let kleene_accepts_six_zeroes () = 
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.kleene int_nfa_3)
       [Some 0; Some 0; Some 0; Some 0; Some 0; Some 0])

let kleene_rejects_four_zeroes () = 
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.kleene int_nfa_3) [Some 0; Some 0; Some 0; Some 0])

let intersection_accepts_even_ones_even_zeroes () =
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.intersection int_nfa_1 int_nfa_2) [Some 0; Some 1; Some 0; Some 1])

let intersection_rejects_odd_ones () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.intersection int_nfa_1 int_nfa_2) [Some 0; Some 1; Some 0])

let intersection_rejects_odd_zeroes () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.intersection int_nfa_1 int_nfa_2) [Some 1; Some 0; Some 1])

let intersection_rejects_three_zeroes () =
  Alcotest.(check bool) "same bool" false
    (IntNfa.accept (IntNfa.intersection int_nfa_2 int_nfa_3) [Some 0; Some 0; Some 0])

let epsilon_remove_accepts_even_ones () =
  Alcotest.(check bool) "same bool" true
    (IntNfa.accept (IntNfa.epsilon_remove int_nfa_4) [Some 1; Some 1])

let epsilon_remove_rejects_empty_string () =
  Alcotest.(check bool) "same bool" false
    (IntNfa.accept (IntNfa.epsilon_remove int_nfa_4) [Some 1])

module IntDfa = MakeDfa(Intalph)
(* open IntDfa.Nfa *)
open IntDfa

let transition_5 =
  let char_map_1 = CharMap.add 1 1 CharMap.empty in
  let char_map_2 = CharMap.add 0 0 char_map_1 in
  let char_map_3 = CharMap.add 1 0 CharMap.empty in
  let char_map_4 = CharMap.add 0 1 char_map_3 in
  let state_map = Nfa.StateMap.add 0 char_map_2 Nfa.StateMap.empty in
  Nfa.StateMap.add 1 char_map_4 state_map

let int_dfa_1 : IntDfa.t =
  {
    start = 0;
    final = Nfa.StateSet.singleton 0;
    transition = transition_5;
  } 

let minimize_accepts_even_ones () =
  Alcotest.(check bool) "same bool" true
    (IntDfa.Nfa.accept (int_dfa_1 |> IntDfa.minimize_dfa |> IntDfa.dfa_to_nfa) 
       [Some 1; Some 1])

let () =
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
