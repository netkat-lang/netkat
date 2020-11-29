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
    final : States.t;
    transition : (state CharMap.t) StateMap.t
  }

  let dfa_to_nfa dfa =
    let update_char ch st acc =
      Nfa.CharMap.add (Nfa.CharOrdered.Char ch) (States.singleton st) acc in
    let update_state st ch_map acc = 
      let char_map = CharMap.fold update_char ch_map Nfa.CharMap.empty in
      StateMap.add st char_map acc in
    let transition = StateMap.fold update_state dfa.transition StateMap.empty in
    let open Nfa in
    {
      start = States.singleton dfa.start;
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

  let make_final (state_lst : int list) : States.t =
    List.fold_left (fun acc elt -> States.add elt acc) States.empty state_lst

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
      `List (States.fold (fun elt acc -> (`Int elt)::acc) dfa.final []) in
    `Assoc [
      ("start", start_json);
      ("trans", transition_json);
      ("final", final_json);
    ]

  let dfa_to_file dfa file = dfa |> dfa_to_json |> (to_file file)

end