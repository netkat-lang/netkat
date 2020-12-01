open Alphabet
open Format

module MakeNfa (A : Alphabet) = struct

  module IntOrdered = struct

    type t = int

    let compare = Stdlib.compare

  end

  module CharOrdered = struct

    type t = Empty | Char of A.symbol

    let compare c1 c2 = match c1, c2 with
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Char char1, Char char2 -> A.compare char1 char2

  end

  module States = Set.Make(IntOrdered)
  module StateMap = Map.Make(IntOrdered)
  module CharMap = Map.Make(CharOrdered)

  type t = {
    start : States.t;
    final : States.t;
    transition : (States.t CharMap.t) StateMap.t
  } 

  let empty nfa = nfa.start = States.empty

  let combine_nfas m1 m2 = StateMap.union (fun k _ _ -> 
      failwith ("duplicate key, invariant violated" ^ (string_of_int k))) m1 m2

  let find_max k _ acc = max k acc

  let get_max_state nfa1 nfa2 = 
    let map1_max = StateMap.fold find_max nfa1.transition 0 in
    let map2_max = StateMap.fold find_max nfa2.transition 0 in
    max  map1_max map2_max

  let union nfa1 nfa2 =
    let nfa_union = combine_nfas nfa1.transition nfa2.transition in
    let max_state = get_max_state nfa1 nfa2 in
    let start_state = max_state + 1 in
    let final_state = start_state + 1 in
    let old_start_states = States.union nfa1.start nfa2.start in
    let transition = StateMap.add start_state
        (CharMap.add Empty old_start_states (CharMap.empty)) nfa_union in
    let old_final_states = States.union nfa1.final nfa2.final in
    let transition' = States.fold (fun st map -> 
        StateMap.add st
          (CharMap.add Empty (States.singleton final_state) CharMap.empty) map)
        old_final_states transition in
    {
      start = States.singleton start_state;
      final = States.singleton final_state;
      transition = transition'
    }

  let intersection nfa1 nfa2 = 
    let nfa_union = combine_nfas nfa1.transition nfa2.transition in
    let transition = States.fold (fun st map ->
        StateMap.add st (CharMap.add Empty nfa2.start CharMap.empty) map) 
        nfa1.final nfa_union in
    {
      start = nfa1.start;
      final = nfa2.final;
      transition = transition
    }

  let kleene nfa = 
    let max_state = StateMap.fold find_max nfa.transition 0 in
    let start_state = max_state + 1 in
    let final_state = start_state + 1 in
    let cycle_added = States.fold (fun st acc ->
        StateMap.add st (CharMap.add Empty nfa.start CharMap.empty) acc) 
        nfa.final nfa.transition in
    let final_transition_added = States.fold (fun st acc ->
        StateMap.add st (CharMap.add Empty (States.singleton final_state) CharMap.empty) acc)
        nfa.final cycle_added in
    let start_transition_added = StateMap.add start_state 
        (CharMap.add Empty (States.add final_state nfa.start) CharMap.empty) 
        final_transition_added in
    {
      start = States.singleton start_state;
      final = States.singleton final_state;
      transition = start_transition_added
    }

  let accept nfa str =
    let rec step st = function
      | [] -> States.is_empty (States.inter nfa.final st)
      | h::t -> let next_states = States.fold (fun st acc ->
          let st_tbl = StateMap.find st nfa.transition in
          States.union acc (CharMap.find h st_tbl)) st States.empty in
        step next_states t in
    step nfa.start str

  (*let to_dot nfa file = 
    let dot_lst_chars curr ch (next_states : States.t) acc =
      let edge_label = match ch with
        | CharOrdered.Char c -> A.to_string c
        | CharOrdered.Empty -> "eps" in
      States.fold (fun st acc ->
          let edge = (string_of_int curr) ^  " -> " ^ (string_of_int st) ^ 
                     " [ label= \" " ^ edge_label ^ " \" ]; " in
          edge::acc) next_states acc in
    let dot_lst_states st char_map acc =
      CharMap.fold (dot_lst_chars st) char_map acc in
    match nfa with
    | {start; final; transition} ->
      let transitions = StateMap.fold dot_lst_states transition [] in
      let dot_lst = ["digraph D {"] @ transitions @ ["}"] in
      let out_ch = open_out file in
      let fmt = formatter_of_out_channel out_ch in
      pp_print_list (fun fmt elt -> pp_print_string fmt elt;
                      pp_print_newline fmt ()) fmt dot_lst;
      pp_print_flush fmt ();
      close_out out_ch *)

end 