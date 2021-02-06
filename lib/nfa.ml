open Alphabet

module MakeNfa (A : Alphabet) = struct

  module CharOrdered = struct

    type t = A.symbol option

    let compare c1 c2 = Option.compare A.compare c1 c2

  end

  module StateSet = Set.Make(Int)
  module StateMap = Map.Make(Int)
  module CharMap = Map.Make(CharOrdered)

  type t = {
    start : StateSet.t;
    final : StateSet.t;
    transition : (StateSet.t CharMap.t) StateMap.t
  } 

  let print_transitions nfa =
    StateMap.iter (fun st char_map -> 
        print_endline ("state: " ^ (string_of_int st));
        CharMap.iter (fun ch states -> 
            let char_str = match ch with
              | None -> "eps"
              | Some c -> A.to_string c in
            print_endline ("char: " ^ char_str);
            StateSet.iter (fun st -> print_endline (string_of_int st)) states;
            print_endline "" 
          ) 
          char_map; print_endline "") nfa.transition

  let find_in_state_map map key default =
    match StateMap.find_opt key map with
    | Some value -> value 
    | None -> default

  let find_in_char_map map key default = 
    match CharMap.find_opt key map with 
    | Some value -> value 
    | None -> default

  let empty nfa = nfa.start = StateSet.empty

  let combine_nfas m1 m2 = StateMap.union (fun k _ _ -> 
      failwith ("duplicate key, invariant violated " ^ (string_of_int k))) m1 m2

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
    let old_start_states = StateSet.union nfa1.start nfa2.start in
    let transition = StateMap.add start_state
        (CharMap.add None old_start_states (CharMap.empty)) nfa_union in
    let old_final_states = StateSet.union nfa1.final nfa2.final in
    let transition' = StateSet.fold (fun st map -> 
        let init_char_map = find_in_state_map map st CharMap.empty in
        StateMap.add st
          (CharMap.add None (StateSet.singleton final_state) init_char_map) map)
        old_final_states transition in
    {
      start = StateSet.singleton start_state;
      final = StateSet.singleton final_state;
      transition = transition'
    }

  let get_all_states nfa =
    StateMap.fold (fun st char_map acc ->
        CharMap.fold (fun ch states total -> 
            StateSet.union states total) char_map (StateSet.add st acc)
      ) nfa.transition StateSet.empty

  let get_alphabet nfa =
    let char_map = nfa.transition |> StateMap.choose |> snd in
    char_map |> CharMap.bindings |> (List.map fst)

  let map_state_pairs nfa1 nfa2 start =
    let count = ref start in
    let tbl = Hashtbl.create 10 in
    let nfa1_states = get_all_states nfa1 in
    let nfa2_states = get_all_states nfa2 in
    StateSet.iter (fun st1 -> StateSet.iter (fun st2 ->
        Hashtbl.add tbl (st1, st2) !count; count := !count + 1) 
        nfa2_states) nfa1_states; tbl

  let create_transition pair_state st1 st2 char_map1 char_map2 curr tbl =
    let char_map_new = CharMap.fold (fun ch states1 acc ->
        match CharMap.find_opt ch char_map2 with
        | Some states2 -> 
          StateSet.fold (fun st1' acc1 -> 
              StateSet.fold (fun st2' acc2 -> 
                  let next_state = Hashtbl.find tbl (st1', st2') 
                                   |> StateSet.singleton in
                  CharMap.add ch next_state acc2) states2 acc1) states1 acc
        | None -> acc) char_map1 CharMap.empty in
    StateMap.add pair_state char_map_new curr

  let make_intersection_transitions nfa1 nfa2 tbl =
    StateMap.fold (fun st1 char_map1 acc1 -> 
        StateMap.fold (fun st2 char_map2 acc2 ->
            let pair_state = Hashtbl.find tbl (st1, st2) in
            create_transition pair_state st1 st2 char_map1 char_map2 acc2 tbl) 
          nfa2.transition acc1) nfa1.transition StateMap.empty

  let get_intersection_start_final_states nfa1 nfa2 tbl =
    StateMap.fold (fun st1 _ acc1 -> 
        StateMap.fold (fun st2 _ acc2 -> 
            let pair_state = Hashtbl.find tbl (st1, st2) in
            let start = if StateSet.mem st1 nfa1.start && 
                           StateSet.mem st2 nfa2.start then
                StateSet.add pair_state (fst acc2) else fst acc2 in
            let final = if StateSet.mem st1 nfa1.final &&
                           StateSet.mem st2 nfa2.final then
                StateSet.add pair_state (snd acc2) else snd acc2 in
            start, final) nfa2.transition acc1) 
      nfa1.transition (StateSet.empty, StateSet.empty)

  let intersection nfa1 nfa2 = 
    let max_state = get_max_state nfa1 nfa2 in
    let tbl = map_state_pairs nfa1 nfa2 (max_state + 1) in
    let state_map = make_intersection_transitions nfa1 nfa2 tbl in
    let start, final = get_intersection_start_final_states nfa1 nfa2 tbl in
    {
      start = start;
      final = final;
      transition = state_map;
    }

  let concatenation nfa1 nfa2 = 
    let nfa_union = combine_nfas nfa1.transition nfa2.transition in
    let transition = StateSet.fold (fun st map ->
        StateMap.add st (CharMap.add None nfa2.start CharMap.empty) map) 
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
    let cycle_added = StateSet.fold (fun st acc ->
        StateMap.add st (CharMap.add None nfa.start CharMap.empty) acc) 
        nfa.final nfa.transition in
    let final_transition_added = StateSet.fold (fun st acc ->
        StateMap.add st (CharMap.add None (StateSet.singleton final_state) 
                           CharMap.empty) acc)
        nfa.final cycle_added in
    let start_transition_added = StateMap.add start_state 
        (CharMap.add None (StateSet.add final_state nfa.start) CharMap.empty) 
        final_transition_added in
    {
      start = StateSet.singleton start_state;
      final = StateSet.singleton final_state;
      transition = start_transition_added
    }

  let rec get_next_states_empty state_map tbl curr =
    match CharMap.find_opt None tbl with
    | Some states -> 
      if StateSet.subset states curr then curr else
        let new_states = StateSet.diff states curr in
        let new_curr = StateSet.union states curr in
        StateSet.fold (fun st acc -> 
            match StateMap.find_opt st state_map with 
            | Some tbl' -> get_next_states_empty state_map tbl' acc
            | None -> acc
          ) new_states new_curr
    | None -> curr

  let get_next_states_char state_map ch tbl curr =
    let states = find_in_char_map tbl ch StateSet.empty in
    StateSet.union states curr

  let transition_from_empty nfa st = 
    StateSet.fold (fun st acc ->
        match StateMap.find_opt st nfa.transition with
        | Some st_tbl -> get_next_states_empty nfa.transition st_tbl acc
        | None -> acc) st st

  let transition_from_char nfa char init_states =
    StateSet.fold (fun st acc ->
        match StateMap.find_opt st nfa.transition with
        | Some st_tbl -> get_next_states_char nfa.transition char st_tbl acc 
        | None -> acc)
      init_states StateSet.empty

  let accept nfa str =
    let rec step st = function
      | [] -> 
        let final_states = transition_from_empty nfa st in
        StateSet.disjoint nfa.final final_states |> not 
      | h::t -> 
        let init_states = transition_from_empty nfa st  in
        let next_states = transition_from_char nfa h init_states in
        let final_states = transition_from_empty nfa next_states in
        step final_states t in
    step nfa.start str

  let get_epsilon_transitions_from_char nfa closure st 
      (transitions, start, final) ch =
    if ch = None then (transitions, start, final) else
      begin
        let next_states = transition_from_char nfa ch closure in
        let char_transition = match StateMap.find_opt st transitions with
          | Some char_map -> CharMap.add ch next_states char_map
          | None -> CharMap.add ch next_states CharMap.empty in
        let transitions' = StateMap.add st char_transition transitions in
        let final' = if StateSet.disjoint closure final then final else  
            StateSet.add st final in
        let start' = if StateSet.disjoint closure start then start else
            StateSet.add st start in
        (transitions', start', final')
      end

  let get_epsilon_transitions nfa alphabet epsilon_closures =
    Hashtbl.fold (fun st closure (transitions, start, final) ->
        List.fold_left (get_epsilon_transitions_from_char nfa closure st) 
          (transitions, start, final)
          alphabet) epsilon_closures (StateMap.empty, nfa.start, nfa.final)

  let epsilon_remove nfa =
    let states = get_all_states nfa in
    let epsilon_closures = Hashtbl.create 10 in
    StateSet.iter (fun st -> 
        Hashtbl.add epsilon_closures st 
          (transition_from_empty nfa (StateSet.singleton st))) states;
    let alphabet = get_alphabet nfa in
    let (transitions, start, final) = get_epsilon_transitions nfa alphabet 
        epsilon_closures in
    let nfa' = 
      {
        start = start;
        final = final;
        transition = transitions;
      } in
    print_transitions nfa';
    nfa'

  let equivalence nfa1 nfa2 = failwith "unimplemented"

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
