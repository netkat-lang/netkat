open Fa

module MakeNfa (A : Alphabet) = struct

  module CharOrdered = struct

    type t = Empty | Char of A.t

    let compare c1 c2 = match c1, c2 with
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Char char1, Char char2 -> A.compare char1 char2
  end

  module States = Set.Make(Int32)
  module StateMap = Map.Make(Int32)
  module CharMap = Map.Make(CharOrdered)

  type t = {
    start : States.t;
    final : States.t;
    transition : (States.t CharMap.t) StateMap.t
  } 

  let empty nfa = nfa.start = States.empty

  let combine_nfas m1 m2 = StateMap.union (fun k _ _ -> 
      failwith ("duplicate key, invariant violated" ^ (Int32.to_string k))) m1 m2

  let find_max k _ acc = max (Int32.to_int k) (Int32.to_int acc) |> Int32.of_int

  let get_max_state nfa1 nfa2 = 
    let map1_max = StateMap.fold find_max nfa1.transition Int32.zero in
    let map2_max = StateMap.fold find_max nfa2.transition Int32.zero in
    max (Int32.to_int map1_max) (Int32.to_int map2_max) |> Int32.of_int

  let union nfa1 nfa2 =
    let nfa_union = combine_nfas nfa1.transition nfa2.transition in
    let max_state = get_max_state nfa1 nfa2 in
    let start_state = Int32.succ max_state in
    let final_state = Int32.succ start_state in
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
    let max_state = StateMap.fold find_max nfa.transition Int32.zero in
    let start_state = Int32.succ max_state in
    let final_state = Int32.succ start_state in
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

  let to_dot nfa = failwith "unimplemented"

end