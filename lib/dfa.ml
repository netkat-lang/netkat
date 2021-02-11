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

  (* For modifying the machine in Rx.of_dfa.
     Suspect there is a cleaner way that doesn't require the messy offset *)
  let new_state (dfa:t) (offset:int) =
    let find_max k _ acc = max k acc in
    offset + StateMap.fold find_max dfa.transition 0

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

  let mk_dfa (trans) (final_lst: int list) =
    {
      start = 0;
      final = List.fold_left (fun acc elt -> StateSet.add elt acc) 
          StateSet.empty final_lst;
      transition = make_transitions trans
    }

  let make_final (state_lst : int list) : StateSet.t =
    List.fold_left (fun acc elt -> StateSet.add elt acc)
      StateSet.empty state_lst

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

  let dfa_to_channel dfa out_ch = dfa |> dfa_to_json |> (to_channel out_ch)

  let get_alphabet dfa =
    let char_map = dfa.transition |> StateMap.choose |> snd in
    char_map |> CharMap.bindings |> (List.map fst)

  let get_states dfa = StateMap.fold (fun st _ acc -> StateSet.add st acc) 
      dfa.transition StateSet.empty 

  let complement dfa =
    let states = get_states dfa in
    {dfa with final = StateSet.diff states dfa.final}

  let intersection d1 d2 = Nfa.intersection (dfa_to_nfa d1) (dfa_to_nfa d2)

  let difference d1 d2 = intersection d1 (complement d2)

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

  let get_max_state (nfa : Nfa.t) = 
    StateMap.fold (fun k _ acc -> max k acc) nfa.transition 0

  module PowerSet = Set.Make(StateSet)
  let superset xs = 
    StateSet.fold (fun x ps -> 
        PowerSet.fold (fun ss -> PowerSet.add (StateSet.add x ss)) ps ps) xs 
      (PowerSet.singleton StateSet.empty)

  let get_state_powerset nfa start =
    let count = ref start in
    let tbl = Hashtbl.create 10 in
    let states = Nfa.get_all_states nfa in
    let powerset = superset states in
    PowerSet.iter (fun subset -> 
        Hashtbl.add tbl subset !count; count := !count + 1) powerset; tbl

  let determinize_transition_update nfa tbl current_states current_states'
      queue transition visited char =
    let char' = match char with
      | Some ch -> ch
      | None -> failwith "postcondition of epsilon remove violated" in
    let next_states = Nfa.transition_from_char nfa char current_states in 
    let next_states' = Hashtbl.find tbl next_states in 
    let char_map = match StateMap.find_opt current_states' !transition with
      | None -> CharMap.empty
      | Some char_map -> char_map in
    let char_map' = CharMap.add char' next_states' char_map in
    transition := StateMap.add current_states' char_map' !transition;
    if not (PowerSet.mem next_states !visited) then
      begin
        visited := PowerSet.add next_states !visited;
        Queue.add next_states queue
      end else ()

  let determinize (nfa : Nfa.t) =
    let nfa' = Nfa.epsilon_remove nfa in
    let tbl = get_state_powerset nfa' (get_max_state nfa' + 1) in
    let queue = Queue.create () in
    let visited = ref PowerSet.empty in
    let transition = ref StateMap.empty in
    let alphabet = Nfa.get_alphabet nfa' in
    Queue.add nfa'.start queue;
    while not (Queue.is_empty queue)
    do
      let current_states = Queue.pop queue in
      let current_states' = Hashtbl.find tbl current_states in
      List.iter (determinize_transition_update nfa' tbl current_states
                   current_states' queue transition visited) alphabet
    done;
    {
      start = Hashtbl.find tbl nfa'.start;
      final = Hashtbl.find tbl nfa'.final |> StateSet.singleton;
      transition = !transition
    }

  let get_next_state dfa st ch = 
    CharMap.find ch (Nfa.StateMap.find st dfa.transition)

  let find_counterexample dfa1 dfa2 =
    let queue = Queue.create () in
    Queue.push (dfa1.start, dfa2.start, []) queue;
    let dfa1_states = get_states dfa1 |> StateSet.elements in
    let dfa2_states = get_states dfa2 |> StateSet.elements in
    let visited = Hashtbl.create
        ((List.length dfa1_states) * (List.length dfa2_states)) in
    let counterexample = ref None in
    while not (Queue.is_empty queue) do
      let (s_t, s_l, str) = Queue.pop queue in
      let visit_next_states character =
        let s_t' = get_next_state dfa1 s_t character in
        let s_l' = get_next_state dfa2 s_l character in
        (if Hashtbl.mem visited (s_t', s_l') then () else
         if StateSet.mem s_t' dfa1.final then 
           begin
             (* dfa1 and dfa2 are at final state *)
             if StateSet.mem s_l' dfa2.final then 
               Queue.add (s_t', s_l', str @ [Some character]) queue else
             if !counterexample = None then 
               counterexample := Some (str @ [Some character])
             else ()
           end else
         if StateSet.mem s_l' dfa2.final then
           begin
             if !counterexample = None then 
               counterexample := Some (str @ [Some character])
             else ()
           end
         else 
           (* dfa1 and dfa2 and at non-final states *)
           Queue.add (s_t', s_l', str @ [Some character]) queue);
        Hashtbl.add visited (s_t', s_l') () in
      List.iter visit_next_states (get_alphabet dfa1)
    done; !counterexample

  let equivalence dfa1 dfa2 =
    find_counterexample dfa1 dfa2 = None

  let rep_symlist (dfa:t) : A.symbol list option =
    (* Perform a BFS to get a representative string
       q: Queue of (acc, state) pairs where acc is the string needed to get
       from start to this state*)
    let rec explore (q:(A.symbol list*state) list) (visited: StateSet.t) : A.symbol list option =
      match q with
      | [] -> None
      | (acc,s)::rem ->
        if StateSet.mem s visited then
          explore rem visited
        else if StateSet.mem s dfa.final then
          Some acc
        else
          let trans = StateMap.find s dfa.transition in
          let added = CharMap.fold (fun c ns a -> (acc @ [c],ns)::a) trans [] in
          explore (rem @ added) (StateSet.add s visited) in
    explore [([], dfa.start)] StateSet.empty

  let representative (dfa:t) : string =
    let r = rep_symlist dfa in
    match r with
    | None -> failwith "representative: Empty language" 
    | Some s -> Core_kernel.(String.concat ~sep:"" (List.map ~f:(A.to_string) s)) 

end
