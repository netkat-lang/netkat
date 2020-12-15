(* open Alcotest *)
open Nerode
open Nfa

module IntAlphabet = struct

  type symbol = int
  type t = int list

  let compare = Stdlib.compare

  let iter = List.iter

  let extract_json = function
    | `Int i -> i
    | _ -> failwith "invalid json"

  let to_json i = `Int i

  let to_string = string_of_int

end

module IntNfa = MakeNfa(IntAlphabet)
open IntNfa

(* accepts even number of 1s *)
let transition_1 =
  let char_map_1 = CharMap.add (Char 1) (States.singleton 1) CharMap.empty in
  let char_map_2 = CharMap.add (Char 0) (States.singleton 0) char_map_1 in
  let char_map_3 = CharMap.add (Char 1) (States.singleton 0) CharMap.empty in
  let char_map_4 = CharMap.add (Char 0) (States.singleton 1) char_map_3 in
  let state_map = StateMap.add 0 char_map_2 StateMap.empty in
  StateMap.add 1 char_map_4 state_map

(* accepts even number of 0s *)
let transition_2 = 
  let char_map_1 = CharMap.add (Char 1) (States.singleton 2) CharMap.empty in
  let char_map_2 = CharMap.add (Char 0) (States.singleton 3) char_map_1 in
  let char_map_3 = CharMap.add (Char 1) (States.singleton 3) CharMap.empty in
  let char_map_4 = CharMap.add (Char 0) (States.singleton 2) char_map_3 in
  let state_map = StateMap.add 2 char_map_2 StateMap.empty in
  StateMap.add 3 char_map_4 state_map

let int_nfa_1 = 
  {
    start = States.add 0 States.empty;
    final = States.add 0 States.empty;
    transition = transition_1;
  }

let int_nfa_2 = 
  {
    start = States.add 2 States.empty;
    final = States.add 2 States.empty;
    transition = transition_2;
  }

let int_nfa_empty =
  {
    start = States.empty;
    final = States.empty;
    transition = StateMap.empty
  }

let empty () =
  Alcotest.(check bool) "same bool" true (IntNfa.empty int_nfa_empty)

let not_empty () =
  Alcotest.(check bool) "same bool" false (IntNfa.empty int_nfa_1)

let reject_two_ones () =
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept int_nfa_1 [Char 1; Char 1])

let reject_odd_ones () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept int_nfa_1 [Char 1; Char 1; Char 1])

let reject_many_zeroes () =
  Alcotest.(check bool) "same bool" true
    (IntNfa.accept int_nfa_1 [Char 1; Char 0; Char 1; Char 0])

let union_accepts_even_zeroes () =
  Alcotest.(check bool) "same bool" true 
    (IntNfa.accept (IntNfa.union int_nfa_1 int_nfa_2) [Char 0; Char 0])

let union_rejects_odd_ones_and_zeroes () =
  Alcotest.(check bool) "same bool" false 
    (IntNfa.accept (IntNfa.union int_nfa_1 int_nfa_2) [Char 1; Char 0])

let () =
  Alcotest.run "Nfa"
    [
      ( "empty",
        [
          Alcotest.test_case "empty" `Quick empty;
          Alcotest.test_case "not empty" `Quick not_empty;
        ] );
      ( "accept",
        [ 
          Alcotest.test_case "accept 11" `Quick reject_two_ones;
          Alcotest.test_case "accept 1010" `Quick reject_many_zeroes; 
          Alcotest.test_case "reject 111" `Quick reject_odd_ones;
        ]);
      ( "union",
        [ 
          Alcotest.test_case "accept 00" `Quick union_accepts_even_zeroes;
          Alcotest.test_case "reject 10" `Quick union_rejects_odd_ones_and_zeroes;
        ]);


    ]