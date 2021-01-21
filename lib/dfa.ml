open Yojson.Basic
open Yojson.Basic.Util
open Nfa
open Alphabet

module MakeDfa (A : Alphabet) = struct

  module Nfa = MakeNfa(A)
  open Nfa

  type state = int

  module CharOrdered = struct

    type t = A.symbol

    let compare = A.compare

  end

  module CharMap = Map.Make(CharOrdered)

  type t = {
    start : state;
    final : StateSet.t;
    transition : (state CharMap.t) StateMap.t
  }

  let dfa_to_nfa dfa =
    let update_char ch st acc =
      Nfa.CharMap.add (Some ch) (StateSet.singleton st) acc in
    let update_state st ch_map acc = 
      let char_map = CharMap.fold update_char ch_map Nfa.CharMap.empty in
      StateMap.add st char_map acc in
    let transition = StateMap.fold update_state dfa.transition StateMap.empty in
    let open Nfa in
    {
      start = StateSet.singleton dfa.start;
      final = dfa.final;
      transition = transition
    }

  let to_string (ch_lst : A.symbol list) : string =
    List.fold_left (fun acc elt -> 
        acc ^ (A.to_string elt)) "" ch_lst

  let make_transitions trans_lst : (state CharMap.t) StateMap.t =
    let extracted_lst = List.fold_left 
        (fun acc elt -> 
           match elt with 
           | `List [`Int start; ch; `Int next] -> 
             (start, A.extract_json ch, next)::acc
           | _ -> failwith "invalid json format") [] trans_lst in
    let transition_helper state_map (start, char, next) =
      match StateMap.find_opt start state_map with
      | Some char_map -> 
        let char_map' = CharMap.add char next char_map in
        StateMap.add start char_map' state_map 
      | None ->
        let char_map = CharMap.empty in
        let char_map' = CharMap.add char next char_map in
        StateMap.add start char_map' state_map in
    List.fold_left transition_helper StateMap.empty extracted_lst

  let make_final (state_lst : int list) : StateSet.t =
    List.fold_left (fun acc elt -> StateSet.add elt acc) StateSet.empty state_lst

  let json_to_dfa json = 
    {
      start = json |> member "start" |> to_int;
      transition = json |> member "trans" |> to_list |> make_transitions;
      final = json |> member "final" |> to_list |> filter_int |> make_final;
    }

  let make_transition_json dfa state char_map (acc : Yojson.Basic.t list) =
    let transition_from_char ch next acc =
      `List [`Int state; A.to_json ch; `Int next]::acc in 
    CharMap.fold transition_from_char char_map acc

  let dfa_to_json dfa =
    let start_json = `Int dfa.start in
    let transition_json = 
      `List (StateMap.fold (make_transition_json dfa) dfa.transition []) in
    let final_json = 
      `List (StateSet.fold (fun elt acc -> (`Int elt)::acc) dfa.final []) in
    `Assoc [
      ("start", start_json);
      ("trans", transition_json);
      ("final", final_json);
    ]

  let dfa_to_file dfa file = dfa |> dfa_to_json |> (to_file file)

  let get_alphabet dfa =
    let char_map = dfa.transition |> StateMap.choose |> snd in
    char_map |> CharMap.bindings |> (List.map fst)

  let get_states dfa = StateMap.fold (fun st _ acc -> StateSet.add st acc) 
      dfa.transition StateSet.empty 

  let complement dfa =
    let states = get_states dfa in
    {dfa with final = StateSet.diff states dfa.final}

  module MinimizeOrdered = struct

    type t = state * state

    let compare (a, b) (c, d) =
      if a = c && b = d then 0 else
      if a = d && b = c then 0 else -1

  end

  module MinimizeSet = Set.Make(MinimizeOrdered)

  let rec minimize dfa marked unmarked = 
    let mark_pairs (s1, s2) (b, m, u) =
      if b then (b, m, u) else
        begin
          let char_set1 = StateMap.find s1 dfa.transition in
          let char_set2 = StateMap.find s2 dfa.transition in
          CharMap.fold (fun ch st (b, m', u') -> 
              if b then (b, m', u') else
                let next_state = CharMap.find ch char_set2 in
                if MinimizeSet.mem (next_state, st) m' then
                  (* mark s1 and s2 *)
                  (true, 
                   MinimizeSet.add (s1, s2) m',
                   MinimizeSet.remove (s1, s2) u') else
                  (b, m', u')) 
            char_set1 (b, m, u)
        end in
    let (b, m, u) = MinimizeSet.fold 
        mark_pairs unmarked (false, marked, unmarked) in
    if b then minimize dfa m u else u

  let minimize_dfa dfa = 
    let states = get_states dfa in
    let unmarked = StateSet.fold (fun st1 acc1 -> 
        StateSet.fold (fun st2 acc2 -> MinimizeSet.add (st1, st2) acc2)
          states acc1) states MinimizeSet.empty in
    let (marked, unmarked') = MinimizeSet.fold (fun (st1, st2) (m, u) -> 
        let st1_final = StateSet.mem st1 dfa.final in
        let st2_final = StateSet.mem st2 dfa.final in
        if (st1_final && (not st2_final)) || (st2_final && (not st1_final)) then
          (MinimizeSet.add (st1, st2) m, MinimizeSet.remove (st1, st2) u) 
        else (m, u)) unmarked (MinimizeSet.empty, MinimizeSet.empty) in
    let unmarked'' = (minimize dfa) marked unmarked' in
    let remove_state_transitions (st1, st2) acc =
      StateMap.fold (fun st ch_map acc ->
          if st = st2 then StateMap.remove st acc else
            let ch_map' = CharMap.fold (fun ch state acc -> 
                if state = st2 then CharMap.add ch st1 acc else acc
              ) ch_map ch_map in
            StateMap.add st ch_map' acc
        ) acc acc in
    MinimizeSet.fold remove_state_transitions unmarked'' dfa.transition

end