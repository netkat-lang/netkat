open Yojson.Basic
open Yojson.Basic.Util


type state = int

type character = Empty | Char of int

type t = {states: state list;
          start: state; 
          transition: state -> character -> state; 
          final: state list;
          alphabet : character list}

let to_string (ch_lst : character list) : string =
  List.fold_left (fun acc elt -> 
      match elt with
      | Empty -> acc
      | Char e -> acc ^ (string_of_int e)) "" ch_lst

let make_transition_function trans_lst =
  let extracted_lst = List.fold_left 
      (fun acc elt -> acc @ [elt |> to_list |> filter_int]) [] trans_lst in
  let transition_matrix = Hashtbl.create 10 in
  let transition_helper elt =
    let transition_matrix = Hashtbl.create 10 in
    match elt with
    | start::char::next::[] ->
      begin
        match Hashtbl.find_opt transition_matrix start with
        | Some tbl -> Hashtbl.add tbl char next
        | None ->
          begin
            let tbl = Hashtbl.create 10 in
            Hashtbl.add tbl char next;
            Hashtbl.add transition_matrix start tbl
          end
      end    
    | _ -> () in
  List.iter transition_helper extracted_lst;
  let transition_function st ch =
    let tbl = Hashtbl.find transition_matrix st in
    Hashtbl.find tbl ch in                                        
  transition_function

let json_to_dfa json = 
  {
    states =  json |> member "states" |> to_list |> filter_int;
    start = json |> member "start" |> to_int;
    transition = json |> member "trans" |> to_list |> make_transition_function;
    final = json |> member "final" |> to_list |> filter_int;
    alphabet = json |> member "alphabet" |> to_list |> filter_int |> 
               (List.map (fun elt -> Char elt))
  }

let make_transition_json dfa acc state =
  let transition_from_alpha lst alpha =
    match alpha with
    | Char a -> `List [`Int state; `Int a; `Int (dfa.transition state alpha)]::lst
    | Empty -> failwith "invariant violated" in
  List.fold_left transition_from_alpha acc dfa.alphabet

let dfa_to_json dfa =
  let states_json = `List (List.map (fun elt -> `Int elt) dfa.states) in
  let start_json = `Int dfa.start in
  let transition_json = `List (List.fold_left (make_transition_json dfa) [] dfa.states) in
  let final_json = `List (List.map (fun elt -> `Int elt) dfa.final) in
  let alphabet_json = `List (List.map (fun elt -> 
      match elt with
      | Char c -> `Int c
      | Empty -> failwith "rep invariant violated") dfa.alphabet) in
  `Assoc [
    ("states", states_json);
    ("start", start_json);
    ("trans", transition_json);
    ("final", final_json);
    ("alphabet", alphabet_json)
  ]

let dfa_to_file dfa file = dfa |> dfa_to_json |> (to_file file)
