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
  | Intersect s1, Intersect s2 -> List.compare compare s1 s2

(* Syntactic equivalence *)
and eq (r1:t) (r2:t) = ((compare r1 r2) = 0)

(* Sort and remove adjacent duplicates *)
let usort (lst: t list) : t list =
  List.sort compare lst |>
  List.fold_left (fun r x ->
    match r with
    | [] -> [x]
    | p::rem -> if eq x p then r else x::r
    ) [] |>
  List.rev

(** We have to implement ACI here so that taking derivatives terminates.*)
let rec union_pair (r1:t) (r2:t) : t =
  match r1,r2 with
  | Drop, _ -> r2
  | _, Drop -> r1
  | Star r, Skip
  | Skip, Star r -> Star r
  | Union t1, Union t2 -> Union (usort (t1 @ t2))
  | Union t1, _ -> if List.exists (fun x -> eq x r2) t1 then
                      r1
                   else
                     Union (usort (r2::t1))
  | _, Union t2 -> if List.exists (fun x -> eq x r1) t2 then
                      r2
                   else
                     Union (usort (r1::t2))
  | _, _ -> if eq r1 r2 then r1 else Union (usort [r1;r2])
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

let diff (r1:t) (r2:t) : t = failwith ("TODO: " ^ __LOC__)
  (*
  intersect_pair r1 (neg r2)
  *)

let xor (r1:t) (r2:t) : t =
  union_pair (diff r1 r2) (diff r2 r1)


let rec neg (e: t) = match e with
  | Drop -> Skip
  | Skip -> Drop
  | Dup -> failwith "Negation undefined for dup"
  | Filter (b,f,v) -> Filter (not b, f, v)
  | Mod (f,v) -> failwith "Negation undefined for mod"
  | Seq es -> List.map neg es |> union
  | Union es -> List.map neg es |> seq
  | Star e -> Star (neg e)
  | Intersect es -> List.map neg es |> intersect

(* --- Pretty print --- *)

let to_string (nk: t) : string =
  (* TODO: we likely need more precedences here... *)
  let prec (r:t) : int =
    match r with
    | Union _ -> 1
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
    | Intersect e0 -> String.concat "&" (List.map (to_string_parent (prec e)) e0)
    (*| Neg e0 -> (to_string_parent (prec e) e0) ^ "^"*)
    | Dup -> "dup"
    | Filter (b,f,v) -> (get_or_fail_fid f) ^ (if b then "=" else "≠") ^ (string_of_val v)
    | Mod (f,v) -> (get_or_fail_fid f) ^ "\u{2190}" ^ (string_of_val v)
    in

    if (prec e) < parent_prec then "(" ^ s ^ ")" else s in

  to_string_parent 0 nk

(*

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
