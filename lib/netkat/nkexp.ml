(* AST for NKPL program *)

open Pk

type t =
  | Drop
  | Skip
  | Dup
  | Filter of bool * field * value
  | Mod of field * value
  | Seq of t list
  | Union of t list
  | Star of t
  | Intersect of t list
  | Neg of t
  | Fwd of t
  | Bwd of t
  (* | Range of *) (* TODO *)
  | Exists of field * t
  | Forall of field * t


let skip = Skip
let drop = Drop
let dup = Dup
let filter b f v = Filter (b,f,v)
let modif f v = Mod (f,v)


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
      if f1 = f2 then cmp_value v1 v2 else cmp_field f1 f2
  | Filter (_,_,_), _ -> -1
  | _, Filter (_,_,_) -> 1
  | Mod (f1,v1), Mod (f2,v2)->
      if f1 = f2 then cmp_value v1 v2 else cmp_field f1 f2
  | Mod (_,_), _ -> -1
  | _, Mod (_,_) -> 1
  | Seq lst1, Seq lst2 -> List.compare compare lst1 lst2
  | Seq _, _ -> -1
  | _, Seq _ -> 1
  | Union t1, Union t2 -> List.compare compare t1 t2
  | Union _, _ -> -1
  | _, Union _ -> 1
  | Star s1, Star s2 -> compare s1 s2
  | Star _, _ -> -1
  | _, Star _ -> 1
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
  | Intersect s1, Intersect s2 -> List.compare compare s1 s2
  | Intersect _, _ -> -1
  | _, Intersect _ -> 1
  | Neg s1, Neg s2 -> compare s1 s2

(* Syntactic eqalence *)
and eq (r1:t) (r2:t) = ((compare r1 r2) = 0)

(* Sort and remove adjacent duplicates *)
(*
let usort (lst: t list) : t list =
  List.sort ~compare:compare lst |>
  List.fold_left ~f:(fun r x ->
    match r with
    | [] -> [x]
    | p::rem -> if eq x p then r else x::r
    ) ~init:[] |>
  List.rev
  *)

let union (lst:t list) : t =
  let flatten a x =
    match x with
    | Union u -> u @ a
    | Drop -> a
    | _ -> x::a in
  let nonempty = List.fold_left flatten lst [] in
  match nonempty with
  | [] -> Drop
  | [r] -> r
  | _ -> Union nonempty

let union_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _ -> r2
  | _, Drop -> r1
  | Star r, Skip
  | Skip, Star r -> Star r
  | Union t1, Union t2 -> union (t1 @ t2)
  | Union t1, _ -> if List.exists (fun x -> eq x r2) t1 then r1 else union (r2::t1)
  | _, Union t2 -> if List.exists (fun x -> eq x r1) t2 then r2 else union (r1::t2)
  | _, _ -> if eq r1 r2 then r1 else union [r1;r2]

let seq (lst1:t list) =
  let flatten a x =
    match x with
    | Seq s -> a @ s
    | Skip -> a
    | _ -> a @ [x] in
  let lst = List.fold_left flatten lst1 [] in
  match lst with
  | [] -> Skip
  | [r] -> r
  | _  -> if List.exists (fun x -> eq x Drop) lst then Drop else Seq lst

let seq_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _ -> Drop
  | _, Drop -> Drop
  | Skip, _ -> r2
  | _, Skip -> r1
  | Seq t1, Seq t2 -> Seq (t1 @ t2)
  | Seq t1, _ -> Seq (t1 @ [r2])
  | _, Seq t2 -> Seq (r1::t2)
  | _, _ -> Seq [r1; r2]

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

let neg (r:t) : t =
  match r with
  | Neg s -> s
  | _ -> Neg r

let difference (r1:t) (r2:t) : t =
  intersect_pair r1 (neg r2)

let xor (r1:t) (r2:t) : t =
  union_pair (difference r1 r2) (difference r2 r1)

(* --- Pretty print --- *)

let to_string (nk: t) : string =

  (* TODO: we likely need more precedences here... *)
  let prec (r:t) : int =
    match r with
    | Union _ -> 0
    | Intersect _ -> 1
    | Seq _ -> 2
    | _ -> 3 in

  let rec to_string_parent (parent_prec: int) (r: t) : string =
    let s = match r with
    | Drop  -> "drop"
    | Skip -> "skip"
    | Seq r0 -> String.concat "⋅" (List.map (to_string_parent (prec r)) r0)
    | Union r0 -> String.concat " ∪ " (List.map (to_string_parent (prec r)) r0)
    | Star r0 -> (to_string_parent (prec r) r0) ^ "*"
    | Intersect r0 -> String.concat "&" (List.map (to_string_parent (prec r)) r0)
    | Neg r0 -> (to_string_parent (prec r) r0) ^ "^"
    | Dup -> "dup"
    | Filter (b,f,v) -> (get_or_fail_fid f) ^ (if b then "=" else "≠") ^ (string_of_val v)
    | Mod (f,v) -> (get_or_fail_fid f) ^ "\u{2190}" ^ (string_of_val v)
    | Fwd e -> failwith "TODO"
    | Bwd e -> failwith "TODO"
    | Exists (f, e) -> failwith "TODO"
    | Forall (f, e) -> failwith "TODO"
    in

    if (prec r) < parent_prec then "(" ^ s ^ ")" else s in

  to_string_parent 0 nk

(*
(* --- Brzozowski derivatives --- *)

let rec e (r:t) : bool =
  match r with
  | Drop -> false
  | Skip -> true
  | Char _ -> false
  | Seq r1 -> List.fold_left (List.map ~f:e r1) ~init:true ~f:(&&)
  | Union r1 -> List.fold_left (List.map ~f:e r1) ~init:false ~f:(||)
  | Star _ -> true
  | QMark _ -> true
  | Intersect r0 -> List.fold_left (List.map ~f:e r0) ~init:true ~f:(&&)
  | Neg r0 -> not (e r0)


let rec d (c:symbol) (r0:t) : t =
  match r0 with
  | Drop -> r0
  | Skip -> Drop
  | Char x ->
     if Alphabet.compare c x = 0 then Skip else Drop
  | Seq (r0::tail) ->
     let r0c_r2 = seq_pair (d c r0) (seq tail) in
     if e r0 then
       union_pair r0c_r2 (d c (seq tail))
     else
       r0c_r2
  | Union r -> union (List.map ~f:(d c) r)
  | Star r -> seq_pair (d c r) r0
  | QMark r -> d c r
  | Intersect r -> intersect (List.map ~f:(d c) r)
  | Neg r -> neg (d c r)

  | _ -> failwith "d: improper rx\n%!"

let rec matches (r:t) (u:word) : bool =
  match u with
  | [] ->
     e r
  | c::v ->
     matches (d c r) v

let rec of_word w = match w with
  | [] ->
      Skip
  | c::w ->
      seq_pair (Char c) (of_word w)
*)
