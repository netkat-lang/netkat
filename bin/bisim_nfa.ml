open Core
open Nerode

module CharNfa = Nfa.Make(struct
  module StateSet = Stdlib.Set.Make(Char)
  include Char
  let fresh (s: StateSet.t) = 
    match (StateSet.max_elt s |> Char.to_int) + 1 |> Char.of_int with
    | None -> failwith "Oops, out of characters"
    | Some c -> c
end)

let start = []

(* Bonchi/Pous Fig 3 *)
let f3_alpha = Alphabet.intalph 1
let f3_final = ['x'; 'y'; 'u']
let f3_trans =
  [ ('x', Nfa.Char (Alphabet.sym_of_int 0), 'y')
  ; ('y', Nfa.Char (Alphabet.sym_of_int 0), 'x')
  ; ('x', Nfa.Char (Alphabet.sym_of_int 0), 'z')
  ; ('z', Nfa.Char (Alphabet.sym_of_int 0), 'y')
  ; ('u', Nfa.Char (Alphabet.sym_of_int 0), 'u')
  ]
let fig3 = CharNfa.mk_nfa f3_alpha start f3_final f3_trans

let f5_alpha = Alphabet.intalph 1
let f5_final = [0; 1; 2]
let f5_trans n =
  Array.of_list (



  )

let fig5 n =


let run_bisim upto nfa x1 x2 =
  let s1 = CharNfa.StateSet.singleton x1 in
  let s2 = CharNfa.StateSet.singleton x2 in
  match CharNfa.bisim upto nfa s1 s2 with
  | None -> Printf.printf "Not bisimilar!\n%!"
  | Some r ->
    Printf.printf "Bisimulation found:\n%!";
    CharNfa.StateRel.iter (fun (x, y) ->
      CharNfa.StateSet.iter (fun x' ->
        Printf.printf "%s" (Char.to_string x')) x;
      Printf.printf "--";
      CharNfa.StateSet.iter (fun y' ->
        Printf.printf "%s" (Char.to_string y')) y;
      Printf.printf "\n%!") r

let () =
  Printf.printf "Using naive bisim:\n%!";
  run_bisim CharNfa.naive fig3 'x' 'u';
  Printf.printf "Using up-to congruence bisim:\n%!";
  run_bisim CharNfa.congruence fig3 'x' 'u';
