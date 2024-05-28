(* AST for NKPL program *)

open Pk

type t =
  | Drop
  | Skip
  | Dup
  | Filter of bool * field * value
  | Mod of field * value
  | VFilter of bool * field * string
  | VMod of field * string
  | Seq of t list
  | Union of t list
  | Star of t
  | Intersect of t list
  | Diff of t * t
  | Neg of t
  | Fwd of t
  | Bwd of t
  | Exists of field * t
  | Forall of field * t
  | Var of string
  (* | Lam of  *)
  (* | App of  *)
  (* | Range of *) (* TODO *)

let skip = Skip
let drop = Drop
let dup = Dup
let var s = Var s
let filter b f v = Filter (b,f,v)
let modif f v = Mod (f,v)
let vfilter b f v = VFilter (b,f,v)
let vmodif f v = VMod (f,v)

let rec compare (t1:t) (t2:t) =
  match t1,t2 with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Dup, Dup -> 0
  | Dup, _ -> -1
  | _, Dup -> 1
  | Filter (true,_,_), Filter (false,_,_) -> 1
  | Filter (false,_,_), Filter (true,_,_) -> -1
  | Filter (b1,f1,v1), Filter (b2,f2,v2) ->
      if f1 = f2 then Value.compare v1 v2 else Field.compare f1 f2
  | Filter (_,_,_), _ -> -1
  | _, Filter (_,_,_) -> 1
  | VFilter (true,_,_), VFilter (false,_,_) -> 1
  | VFilter (false,_,_), VFilter (true,_,_) -> -1
  | VFilter (b1,f1,v1), VFilter (b2,f2,v2) ->
      if f1 = f2 then String.compare v1 v2 else Field.compare f1 f2
  | VFilter (_,_,_), _ -> -1
  | _, VFilter (_,_,_) -> 1
  | Mod (f1,v1), Mod (f2,v2)->
      if f1 = f2 then Value.compare v1 v2 else Field.compare f1 f2
  | Mod (_,_), _ -> -1
  | _, Mod (_,_) -> 1
  | VMod (f1,v1), VMod (f2,v2)->
      if f1 = f2 then String.compare v1 v2 else Field.compare f1 f2
  | VMod (_,_), _ -> -1
  | _, VMod (_,_) -> 1
  | Seq lst1, Seq lst2 -> List.compare compare lst1 lst2
  | Seq _, _ -> -1
  | _, Seq _ -> 1
  | Union t1, Union t2 -> List.compare compare t1 t2
  | Union _, _ -> -1
  | _, Union _ -> 1
  | Star s1, Star s2 -> compare s1 s2
  | Star _, _ -> -1
  | _, Star _ -> 1
  | Intersect s1, Intersect s2 -> List.compare compare s1 s2
  | Intersect _, _ -> -1
  | _, Intersect _ -> 1
  | Diff (t1,t2), Diff (t3,t4) -> if compare t1 t3 = 0 then compare t1 t3 else compare t2 t4
  | Diff _, _ -> -1
  | _, Diff _ -> 1
  (*
  | Xor (t1,t2), Xor (t3,t4) -> if compare t1 t3 = 0 then compare t1 t3 else compare t2 t4
  | Xor _, _ -> -1
  | _, Xor _ -> 1
  *)
  | Fwd s1, Fwd s2 -> compare s1 s2
  | Fwd _, _ -> -1
  | _, Fwd _ -> 1
  | Bwd s1, Bwd s2 -> compare s1 s2
  | Bwd _, _ -> -1
  | _, Bwd _ -> 1
  | Exists (f1,s1), Exists (f2,s2) -> if f1 = f2 then compare s1 s2 else Field.compare f1 f2
  | Exists _, _ -> -1
  | _, Exists _ -> 1
  | Forall (f1,s1), Forall (f2,s2) -> if f1 = f2 then compare s1 s2 else Field.compare f1 f2
  | Forall _, _ -> -1
  | _, Forall _ -> 1
  | Neg e1, Neg e2 -> compare e1 e2
  | Neg _, _ -> -1
  | _, Neg _ -> 1
  | Var s1, Var s2 -> String.compare s1 s2

(* Syntactic eqalence *)
and eq (r1:t) (r2:t) = ((compare r1 r2) = 0)

let fwd (t:t) : t = Fwd t
let bwd (t:t) : t = Bwd t
let neg (t:t) : t = Neg t
let exists (f: field) (t:t) : t = Exists (f, t)
let forall (f: field) (t:t) : t = Forall (f, t)
let diff (r1:t) (r2:t) : t = Diff (r1, r2)

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


let xor (r1:t) (r2:t) : t =
  union_pair (diff r1 r2) (diff r2 r1)

let to_string (e: t) : string =
  (* TODO: we likely need more precedences here... *)
  let prec (e0:t) : int =
    match e0 with
    | Fwd _
    | Bwd _
    | Forall _
    | Exists _ -> 0
    | Union _ -> 1
    | Diff _
    (* | Xor _ *)
    | Intersect _ -> 2
    | Seq _ -> 3
    | _ -> 4 in

  let rec to_string_parent (parent_prec: int) (e: t) : string =
    let s = match e with
    | Drop  -> "drop"
    | Skip -> "skip"
    | Seq e0 -> String.concat "⋅" (List.map (to_string_parent (prec e)) e0)
    | Union e0 -> String.concat " ∪ " (List.map (to_string_parent (prec e)) e0)
    | Star e0 -> (to_string_parent (prec e) e0) ^ "*"
    | Intersect e0 -> String.concat " & " (List.map (to_string_parent (prec e)) e0)
    | Dup -> "dup"
    | Filter (b,f,v) -> (Field.get_or_fail_fid f) ^ (if b then "=" else "≠") ^ (Value.to_string v)
    | VFilter (b,f,v) -> (Field.get_or_fail_fid f) ^ (if b then "=" else "≠") ^ v
    | Mod (f,v) -> (Field.get_or_fail_fid f) ^ "\u{2190}" ^ (Value.to_string v)
    | VMod (f,v) -> (Field.get_or_fail_fid f) ^ "\u{2190}" ^ v
    | Neg e0 -> "¬" ^ (to_string_parent (prec e) e0)
    | Var x -> x
    (* | Xor (t1,t2) -> (to_string_parent (prec e) t1) ^ " ⊕ " ^ (to_string_parent (prec e) t2) *)
    | Diff (t1,t2) ->  (to_string_parent (prec e) t1) ^ " - " ^ (to_string_parent (prec e) t2)
    | Fwd e -> "forward " ^ (to_string_parent (prec e) e)
    | Bwd e -> "backward " ^ (to_string_parent (prec e) e)
    | Forall (f,e) -> "forall " ^ (Field.get_or_fail_fid f) ^ " " ^ (to_string_parent (prec e) e)
    | Exists (f,e) -> "exists " ^ (Field.get_or_fail_fid f) ^ " " ^ (to_string_parent (prec e) e)
    in

    if (prec e) < parent_prec then "(" ^ s ^ ")" else s in

  to_string_parent 0 e

let rec eval (env: Env.t) (e: t) : Nk.t =
    match e with
    | Drop  -> Nk.Drop
    | Skip -> Nk.Skip
    | Seq e0 -> List.map (eval env) e0 |> Nk.seq
    | Union e0 -> List.map (eval env) e0 |> Nk.union
    | Star e0 -> eval env e0 |> Nk.star
    | Intersect e0 -> List.map (eval env) e0 |> Nk.intersect
    | Dup -> Nk.dup
    | Filter (b,f,v) -> Nk.filter b f v
    | VFilter (b,f,var) -> Nk.filter b f (Env.lookup_val env var)
    | Mod (f,v) -> Nk.modif f v
    | VMod (f,var) -> Nk.modif f (Env.lookup_val env var)
    | Var x -> Env.lookup_exp env x
    (* | Xor (t1,t2) -> Nk.xor (eval env t1) (eval env t2) *)
    | Diff (t1,t2) -> Nk.diff (eval env t1) (eval env t2)
    | Neg e -> Nk.neg (eval env e)
    | Fwd e -> Nka.forward (eval env e) |> Sp.to_exp
    | Bwd e -> Nka.backward (eval env e) |> Sp.to_exp
    | Forall (f,e) -> begin
                      match e with
                      | Drop
                      | Skip -> eval env e
                      | _ -> failwith ("TODO: " ^ __LOC__)
                      end
    | Exists (f,e) -> failwith ("TODO: " ^ __LOC__)
