open Alphabet
open Core_kernel

module MakeRx (A : Alphabet) = struct

    module Dfa = Dfa.MakeDfa(A)   

    type t = 
      | Empty 
      | Epsilon
      | Char of A.symbol
      | Seq of t list
      | Union of t list
      | Star of t

    let rec compare (t1:t) (t2:t) = 
      match t1,t2 with 
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Epsilon, Epsilon -> 0
      | Epsilon, _ -> -1
      | _, Epsilon -> 1
      | Char s1, Char s2 -> A.compare s1 s2                      
      | Char _, _ -> -1
      | _, Char _ -> 1
      | Seq lst1, Seq lst2 -> List.compare compare lst1 lst2
      | Seq _, _ -> -1
      | _, Seq _ -> 1
      | Union t1, Union t2 -> List.compare compare t1 t2
      | Union _, _ -> -1
      | _, Union _ -> 1
      | Star s1, Star s2 -> compare s1 s2

    and seq (lst1:t list) = 
      let lst = List.filter ~f:(fun x -> not (equiv x Epsilon)) lst1 in
      match lst with
      | [] -> Epsilon
      | [r] -> r
      | _  -> if List.exists ~f:(fun x -> equiv x Empty) lst then Empty else Seq lst

    (* Syntactic equivalence *)
    and equiv (r1:t) (r2:t) = ((compare r1 r2) = 0)

    let union (lst:t list) : t =
      let nonempty = List.filter ~f:(fun x -> not (equiv x Empty)) lst in
      match nonempty with
      | [] -> Empty
      | [r] -> r
      | _ -> Union (List.sort ~compare:compare nonempty)

    let union_pair (r1:t) (r2:t) : t =
      match r1,r2 with 
      | Empty, _ -> r2
      | _, Empty -> r1
      | Star r, Epsilon
      | Epsilon, Star r -> Star r
      | Union t1, Union t2 -> union (t1 @ t2)
      | Union t1, _ -> if List.exists ~f:(fun x -> equiv x r2) t1 then r1 else union (r2::t1)
      | _, Union t2 -> if List.exists ~f:(fun x -> equiv x r1) t2 then r2 else union (r1::t2)
      | _, _ -> if equiv r1 r2 then r1 else union [r1;r2]

    let seq_pair (r1:t) (r2:t) : t =
      match r1,r2 with 
      | Empty, _ -> Empty
      | _, Empty -> Empty
      | Epsilon, _ -> r2
      | _, Epsilon -> r1
      | Seq t1, Seq t2 -> Seq (t1 @ t2)
      | Seq t1, _ -> Seq (t1 @ [r2])
      | _, Seq t2 -> Seq (r1::t2)
      | _, _ -> Seq [r1; r2]

    let star (r0:t) : t =
      match r0 with
      | Epsilon | Empty -> Epsilon
      | Star _ -> r0
      | _ -> Star r0

    let to_string (rx: t) : string =
      let prec (r:t): int =
        match r with
        | Seq t0 -> 1
        | Union t0 -> 0
        | _ -> 2 in

      let rec to_string_parent (parent_prec: int) (r: t) : string =
        let s = match r with
        | Empty  -> "{}"
        | Epsilon -> "e"
        | Char r0 -> A.to_string r0
        | Seq r0 -> String.concat ~sep:"" (List.map ~f:(to_string_parent (prec r)) r0)
        | Union r0 -> String.concat ~sep:"+" (List.map ~f:(to_string_parent (prec r)) r0)
        | Star r0 -> (to_string_parent (prec r) r0) ^ "*" in

        if (prec r) < parent_prec then "(" ^ s ^ ")" else s in

      to_string_parent 0 rx
      
(* --- Brzozowski derivatives --- *)

    let rec e (r:t) : bool = 
      match r with 
      | Empty -> false
      | Epsilon -> true
      | Char _ -> false
      | Seq r1 -> List.fold_left (List.map ~f:e r1) ~init:true ~f:(&&)
      | Union r1 -> List.fold_left (List.map ~f:e r1) ~init:false ~f:(||)
      | Star r1 -> true

    let rec d (c:A.symbol) (r0:t) : t = 
      match r0 with 
      | Empty -> r0
      | Epsilon -> Empty
      | Char x -> 
         if A.compare c x = 0 then Epsilon else Empty
      | Seq (r0::tail) -> 
         let r0c_r2 = seq_pair (d c r0) (seq tail) in
         if e r0 then 
           union_pair r0c_r2 (d c (seq tail))
         else 
           r0c_r2
      | Union r -> union (List.map ~f:(d c) r)
      | Star r -> seq_pair (d c r) r0
      | _ -> failwith "d: improper rx\n%!"

    let rec matches (r:t) (u:A.symbol list) : bool = 
      match u with 
      | [] -> 
         e r 
      | c::v -> 
         matches (d c r) v
  
    (* Representative string for L(R) *)
    let rep_string (r: t) : string =
      let rec rep_symlist (r: t) : A.symbol list =
        match r with
        | Empty -> failwith "rep_string: No representative of the empty language"
        | Epsilon -> []
        | Char x -> [x]
        | Seq lst -> List.concat (List.map ~f:rep_symlist lst)
        | Union (x::tail) -> rep_symlist x
        | Star s -> rep_symlist s
        | _ -> failwith "rep_string: Invalid rx form" (* Union [] *) in
      String.concat ~sep:"" (List.map ~f:A.to_string (rep_symlist r))


(*--- Construct dfa ---*)
    type trans = t * A.symbol * t
    type dfa_graph = t list * trans list

    (* Helper: return the index of the given rx in the given list 
       This seems unnecessarily inefficient.. not sure how to get a map since 
       that would make the module recursive *)
    let index_of (lst:t list) (r:t) =
      let rec idx_of_start (lst:t list) (r:t) (start:int) =
        match lst with
        | [] -> -1
        | t0::tail -> if equiv r t0 then start else idx_of_start tail r (start+1) in
      idx_of_start lst r 0

    let rec dfa_goto (q: t) (c: A.symbol) (states: t list) (delta: trans list) : dfa_graph =
      let qc = d c q in
      if List.exists ~f:(fun q -> equiv q qc) states then
        (states, (q, c, qc)::delta)
      else
        (* Append to preserve the first element of states is the start state *)
        dfa_explore (states @ [qc]) ((q, c, qc)::delta ) qc

    and dfa_explore (states: t list) (delta: trans list) (q: t) : dfa_graph = 
      let acc = (states, delta) in
      A.fold (fun (s,d) c -> (dfa_goto q c s d)) acc

    let to_dfa (r:t): Dfa.t =
      let (states, trans) = dfa_explore [r] [] r in
      let idx (s: t) = index_of states s in
      let final = List.map ~f:idx (List.filter ~f:e states) in
      let trans_to_json (lst: trans list) = 
        let rec trans_to_json_r lst json =
          match lst with
          | [] -> json
          | (r0, x, r1)::tail -> trans_to_json_r tail (`List [`Int (idx r0); A.to_json x; `Int (idx r1)]::json) in
        trans_to_json_r lst [] in
      Dfa.mk_dfa (trans_to_json trans) final

(*--- Dfa to Rx ---*)
    module LabelOrdered = struct
      type t = Dfa.state * Dfa.state

      let compare ((a, b):t) ((c, d):t) =
        if a = c then b - d else a - c
    end
    module LabelMap = Stdlib.Map.Make(LabelOrdered)

    (* Construct Rx from Dfa using Node-elimination *)
    let of_dfa (dfa: Dfa.t) : t =
      let states = Dfa.get_states dfa in

      (* Start with Empty for every pair *)
      let dfa_labels: t LabelMap.t =
        let open Dfa in
        let open Nfa in
        StateSet.fold (fun s1 a1 ->
          StateSet.fold (fun s2 a2 -> 
            LabelMap.add (s1, s2) Empty a2) states a1) states LabelMap.empty |>

      (* Add in the transitions from the DFA *)
        StateMap.fold (fun s cm a1 -> 
          Dfa.CharMap.fold (fun c ns a2 -> 
            (*
            Printf.printf "updating %d %d\n" s ns;
            *)
            LabelMap.update (s, ns) (fun r -> 
              match r with
              | None -> failwith "of_dfa: Found unitialized entry"
              | Some rx -> Some (union_pair rx (Char c)))
            a2) cm a1) dfa.transition in

      (* Add a new start state *)
      let new_start = Dfa.new_state dfa 1 in
      let new_final = Dfa.new_state dfa 2 in

      (* Compute (ss U {new_start}) x (ss U {new_final}) *)
      let states_squared (ss:Dfa.Nfa.StateSet.t) : (Dfa.state * Dfa.state) list =
        let open Dfa in
        let open Nfa in
        let from_set = StateSet.union ss (StateSet.singleton new_start) in
        let to_set = StateSet.union ss (StateSet.singleton new_final) in
        StateSet.fold (fun s1 a1 ->
          StateSet.fold (fun s2 a2 -> (s1,s2)::a2) to_set a1) from_set [] in

      let final_trans (s:Dfa.state) : t =
        let open Dfa in
        let open Nfa in
        if StateSet.mem s dfa.final then Epsilon else Empty in

      (* Add epsilon transition from new start state to old start state, and
      from final states to new final state. *) 
      let extended_labels: t LabelMap.t = 
        let open Dfa in
        let open Nfa in
        StateSet.fold (fun s a -> LabelMap.add (new_start, s) Empty a) states dfa_labels |>
        StateSet.fold (fun s a -> LabelMap.add (s, new_final) (final_trans s) a) states |>
        LabelMap.add (new_start, dfa.start) Epsilon |>
        LabelMap.add (new_start, new_final) Empty in

      (* Now "remove" every original state, updating labels -
         We don't actually remove anything, each round, just update the 
         "remaining" labels to account for the "removed" states *)
      let final_labels: t LabelMap.t =
        let rec eliminate (ss:Dfa.Nfa.StateSet.t) (labels: t LabelMap.t) =

          (* "recreate" s *)
          if Dfa.Nfa.StateSet.is_empty ss then labels else
          let s = Dfa.Nfa.StateSet.min_elt ss in
          let ns = Dfa.Nfa.StateSet.remove s ss in
          let new_labels =
            List.fold_left ~f:(fun a (s1,s2) -> 
              LabelMap.update (s1,s2) (fun r ->
                match r with
                | None -> Printf.printf "Missing: %d %d\n" s1 s2; failwith "of_dfa: Invalid key"
                | Some rx -> Some (union_pair rx (seq [LabelMap.find (s1, s) a;
                                                       star (LabelMap.find (s, s) a);
                                                       LabelMap.find (s, s2) a]))) a)
                             
                ~init:labels (states_squared ns) in
          eliminate ns new_labels in
        eliminate states extended_labels in

      (* All the states are "removed", just look at new_start -> new_final *)
      LabelMap.find (new_start, new_final) final_labels
end
