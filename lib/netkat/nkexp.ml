(* AST for NKPL program *)

open Pk

type t =
  | Exp of Nk.t
  | Neg of t
  | Fwd of t
  | Bwd of t
  | Exists of field * t
  | Forall of field * t
  | Var of string
  (* | Lam of  *)
  (* | Range of *) (* TODO *)

let rec compare (t1:t) (t2:t) =
  match t1,t2 with
  | Fwd s1, Fwd s2 -> compare s1 s2
  | Fwd _, _ -> -1
  | _, Fwd _ -> 1
  | Bwd s1, Bwd s2 -> compare s1 s2
  | Bwd _, _ -> -1
  | _, Bwd _ -> 1
  | Exists (f1,s1), Exists (f2,s2) -> if f1 = f2 then compare s1 s2 else cmp_field f1 f2
  | Exists _, _ -> -1
  | _, Exists _ -> 1
  | Forall (f1,s1), Forall (f2,s2) -> if f1 = f2 then compare s1 s2 else cmp_field f1 f2
  | Forall _, _ -> -1
  | _, Forall _ -> 1
  | Neg e1, Neg e2 -> compare e1 e2
  | Neg _, _ -> -1
  | _, Neg _ -> 1
  | Var s1, Var s2 -> String.compare s1 s2
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Exp e1, Exp e2 -> Nk.compare e1 e2

(* Syntactic eqalence *)
and eq (r1:t) (r2:t) = ((compare r1 r2) = 0)

let fwd (t:t) : t = Fwd t
let bwd (t:t) : t = Bwd t
let neg (t:t) : t = Neg t
let exists (f: field) (t:t) : t = Exists (f, t)
let forall (f: field) (t:t) : t = Forall (f, t)

let rec union_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _ -> r2
  | _, Drop -> r1
  | Star r, Skip
  | Skip, Star r -> Star r
  | Union t1, Union t2 -> Union (t1 @ t2)
  | Union t1, _ -> if List.exists (fun x -> eq x r2) t1 then r1 else Union (r2::t1)
  | _, Union t2 -> if List.exists (fun x -> eq x r1) t2 then r2 else Union (r1::t2)
  | _, _ -> if eq r1 r2 then r1 else Union [r1;r2]
and union lst = List.fold_left union_pair Drop lst

let rec seq_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _ -> Drop
  | _, Drop -> Drop
  | Skip, _ -> r2
  | _, Skip -> r1
  | Seq t1, Seq t2 -> Seq (t1 @ t2)
  | Seq t1, _ -> Seq (t1 @ [r2])
  | _, Seq t2 -> Seq (r1::t2)
  | _, _ -> Seq [r1; r2]
and seq lst = List.fold_left seq_pair Skip lst

let star (r0:t) : t =
  match r0 with
  | Skip | Drop -> Skip
  | Star _ -> r0
  | _ -> Star r0

let intersect (lst:t list) : t =
  let flatten a x =
    match x with
    | Intersect i -> i @ a
    | _ -> x::a in
  let flat = List.fold_left flatten lst [] in
  match flat with
  | [] -> failwith "Nullary intersection undefined"
  | [r] -> r
  | _ -> if List.exists (fun x -> eq x Drop) flat then Drop
         else Intersect flat

let intersect_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _
  | _, Drop -> Drop
  | Star r, Skip
  | Skip, Star r -> Skip
  | Intersect t1, Intersect t2 -> Intersect (t1 @ t2)
  | Intersect t1, _ -> if List.exists (fun x -> eq x r2) t1 then r1 else intersect (r2::t1)
  | _, Intersect t2 -> if List.exists (fun x -> eq x r1) t2 then r2 else intersect (r1::t2)
  | _, _ -> if eq r1 r2 then r1 else intersect [r1;r2]

let difference (r1:t) (r2:t) : t = failwith ("TODO: " ^ __LOC__)
  (*
  intersect_pair r1 (neg r2)
  *)

let xor (r1:t) (r2:t) : t =
  union_pair (difference r1 r2) (difference r2 r1)
let eval (e: t) : Nk.t = failwith ("TODO: " ^ __LOC__)
