(* AST for NKPL program *)

open Pk

type t =
  | Exp of Nk.t
  | Neg of t
  | Fwd of t
  | Bwd of t
  (* | Range of *) (* TODO *)
  | Exists of field * t
  | Forall of field * t

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
  | Neg _, _ -> -1
  | _, Neg _ -> 1
  | Exp e1, Exp e2 -> Nk.compare e1 e2

(* Syntactic eqalence *)
and eq (r1:t) (r2:t) = ((compare r1 r2) = 0)
