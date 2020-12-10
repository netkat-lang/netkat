open Core_kernel
open Alphabet


module rec R : 
sig 
  type t =
    | Empty 
    | Char of Alphabet.symbol
    | Seq of t list
    | Union of S.t
    | Star of t
  val compare : t -> t -> int
end = struct
  type t = 
    | Empty 
    | Char of int
    | Seq of t list
    | Union of S.t
    | Star of t
  let compare r1 r2 = failwith "unimplemented"
end
and S : Set.S with type elt = R.t = S.Make(R)
open R


module MakeRx (A : Alphabet) = struct
    type m = { state_lst: A.symbol list; trans_lst: A.symbol list }

    let alpha = [0; 1]


    let rec compare (t1:t) (t2:t) = 
      match t1,t2 with 
      (*
      | Hole,Hole -> 0
      | _, Hole -> -1
      *)
      | Hole,_ -> 1
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Epsilon, Epsilon -> 0
      | Epsilon, _ -> -1
      | _, Epsilon -> 1
      | Union(Char Zero, Char One), Union(Char Zero, Char One) -> 0
      | Union(Char Zero, Char One), _ -> -1
      | _, Union(Char Zero, Char One) -> 1
      | Char s1, Char s2 -> compare_symbol s1 s2                      
      | Char _, _ -> -1
      | _, Char _ -> 1
      | Seq(t11,t12), Seq(t21, t22) -> 
         let cmp1 = compare t11 t21 in 
         if cmp1 = 0 then 
           compare t12 t22
         else 
           cmp1
      | Seq _, _ -> -1
      | _, Seq _ -> 1
      | Union(t11,t12), Union(t21, t22) -> 
         let cmp1 = compare t11 t21 in 
         if cmp1 = 0 then 
           compare t12 t22
         else 
           cmp1
      | Union _, _ -> -1
      | _, Union _ -> 1
      | QMark t11, QMark t21 -> 
         compare t11 t21
      | QMark _, _ -> -1
      | _, QMark _ -> 1
      | Star t11, Star t21 -> 
         compare t11 t21

    (* Syntactic equivalence with reordering of union *)
    let equiv r1 r2 = (compare r1 r2 == 0)


    (* Construct dfa *)
    let goto (q: A.symbol) (c: A.symbol) (states: A.symbol list) (delta: A.symbol list) =
      let qc = d q c in
      if List.exists ~f:(fun q -> equiv q qc) states then
        { state_lst = states ; trans_lst = [q; c; qc]::delta }
      else
        explore (qc :: states) ([q; c; qc]::delta ) qc

    let explore (states: A.symbol list) (delta: A.symbol list) (q: A.symbol) = 
      let acc = {state_lst = states; trans_lst = delta} in
      List.fold_left (fun acc c -> (goto q c acc.state_list acc.trans_lst)) acc alpha

    let to_dfa (r:t): Dfa.t =
      let qd = explore [r] [] r in
      {
        states =  qd.states_lst;
        start = r;
        transition = make_transition_function qd.trans_lst;
        final = List.filter (fun x -> e x) qd.state_lst;
        alphabet = alpha
      }


    (* pre: r doesn't have a hole *)
    let rec d (r0:t) (c:A.symbol) : t = 
      match r0 with 
      | Empty -> r0
      | Epsilon -> Empty
      | Char d -> 
         if compare_symbol c d = 0 then Epsilon else Empty
      | Seq(r1,r2) -> 
         let r1c_r2 = Seq(d r1 c, r2) in
         if e r1 then 
           Union(r1c_r2, d r2 c)
         else 
           r1c_r2
      | Union(r1,r2) -> 
         Union(d r1 c, d r2 c)
      | QMark(r1) -> 
         d r1 c
      | Star(r1) -> 
         Seq(d r1 c, r0)

    and e (r:t) : bool = 
      match r with 
      | Empty -> false
      | Epsilon -> true
      | Char _ -> false
      | Seq(r1,r2) -> 
         e r1 && e r2
      | Union(r1,r2) -> 
         e r1 || e r2
      | QMark(r1) -> true
      | Star(r1) -> true

    (* pre: r doesn't have a hole *)
    let rec matches (r:t) (u:A.symbols) : bool = 
      incr Stats.match_count;
      match u with 
      | [] -> 
         e r 
      | c::v -> 
         matches (d r c) v
    let union (r1:t) (r2:t) : t =
      match r1,r2 with 
      | Empty, _ -> r2
      | _, Empty -> r1
      | r1, Star r2 when compare r1 r2 = 0 -> Seq(r1,Star r1)                    (* e+e* -> ee* *)
      | Star r1, r2 when compare r1 r2 = 0 -> Seq(r1,Star r1)                    (* e*+e -> ee* *)
      | r1, Union(r2, QMark r3) when compare r1 r3 = 0 -> Union(r2, QMark r1)    (* e1+e2+e1? -> e2+e1? *)
      | r1, Union(QMark r2, r3) when compare r1 r2 = 0 -> Union(QMark r2, r3)    (* e1+e1?+e2 -> e1?+e2 *)
      | r1, Union(r2, Star r3) when compare r1 r3 = 0 -> Union(r2, Star r1)      (* e1+e2+e1* -> e2+e1* *)
      | r1, Union(Star r2, r3) when compare r1 r2 = 0 -> Union(Star r2, r3)      (* e1+e1*+e2 -> e1*+e2 *)
      | QMark r1, Star r2 when compare r1 r2 = 0 -> Star r2                      (* e?+e* -> e* *)
      | Star r1, QMark r2 when compare r1 r2 = 0 -> Star r2                      (* e*+e? -> e* *)
      | QMark r1, r2 when compare r1 r2 = 0 -> QMark r1                          (* e?+e -> e? *)
      | r1, QMark r2 when compare r1 r2 = 0 -> QMark r2                          (* e+e? -> e? *)
      | _, _ -> 
         let cmp = compare r1 r2 in
         if hole_free r1 && cmp = 0 then r1
         else if cmp < 0 then Union(r1,r2)
         else Union(r2,r1)

    let seq (r1:t) (r2:t) =
      match r1, r2 with
      | Epsilon, _ -> r2
      | _, Epsilon -> r1
      | Empty,_ | _, Empty -> Empty
      | Star r1, Star r2 when compare r1 r2 = 0 -> Star r1                       (* e*e* -> e* *)
      | Star r1, QMark r2 when compare r1 r2 = 0 -> Star r1                      (* e*e? -> e* *) 
      | QMark r1, Star r2 when compare r1 r2 = 0 -> Star r2                      (* e?e* -> e* *)
      | _, _ -> Seq(r1,r2)
              
    let star (r0:t) : t =
      match r0 with
      | Epsilon | Empty -> Epsilon
      | QMark r1 -> Star r1
      | Star _ -> r0
      | Seq(r1, Star r2) when compare r1 r2 = 0 -> Star r1                       (* [ee*]* -> e* *)
      | Seq(Star r1, r2) when compare r1 r2 = 0 -> Star r1                       (* [ee*]* -> e* *)
      | Seq(r1, QMark r2) when compare r1 r2 = 0 -> Star r1                      (* [ee?]* -> e* *)
      | Seq(QMark r1, r2) when compare r1 r2 = 0 -> Star r1                      (* [e?e]* -> e* *)
      | _ -> Star r0

    let qmark (r0:t) : t =
       match r0 with
       | Epsilon | Empty -> Epsilon
       | QMark _ -> r0
       | Star r1 -> Star r1
       | Seq(r1, Star r2) when compare r1 r2 = 0 -> Star r1                      (* [ee*]? -> e* *)
       | Seq(Star r1, r2) when compare r1 r2 = 0 -> Star r1                      (* [e*e]? -> e* *)
       | _ -> QMark r0

end
