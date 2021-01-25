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
      | Star t11, Star t21 -> compare t11 t21

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
  

(*--- Construct dfa ---*)
    type trans = t * A.symbol * t
    type dfa_graph = t list * trans list

    (* Helper: return the index of the given rx in the given list *)
    let index_of (lst:t list) (r:t) =
      let rec idx_of_start (lst:t list) (r:t) (start:int) =
        match lst with
        | [] -> -1
        | t0::tail -> if equiv r t0 then start else idx_of_start tail r (start+1) in
      idx_of_start lst r 0

    let rec goto (q: t) (c: A.symbol) (states: t list) (delta: trans list) =
      let qc = d c q in
      if List.exists ~f:(fun q -> equiv q qc) states then
        (states, (q, c, qc)::delta)
      else
        (* Append to preserve the first element of states is the start state *)
        explore (states @ [qc]) ((q, c, qc)::delta ) qc

    and explore (states: t list) (delta: trans list) (q: t) = 
      let acc = (states, delta) in
      A.fold (fun (s,d) c -> (goto q c s d)) acc

    let to_dfa (r:t): Dfa.t =
      let (states, trans) = explore [r] [] r in
      let idx (s: t) = index_of states s in
      let final = List.map ~f:idx (List.filter ~f:e states) in
      let trans_to_json (lst: trans list) = 
        let rec trans_to_json_r lst json =
          match lst with
          | [] -> json
          | (r0, x, r1)::tail -> trans_to_json_r tail (`List [`Int (idx r0); A.to_json x; `Int (idx r1)]::json) in
        trans_to_json_r lst [] in

      Dfa.mk_dfa (trans_to_json trans) final

end
