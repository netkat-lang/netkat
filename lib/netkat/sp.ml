(** SP implementation *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * (t ValueMap.t) * t

let skip = Skip
let drop = Drop

let rec union_pair (t1:t) (t2:t) : t =
  match t1,t2 with
  | Skip, _
  | _, Skip -> Skip
  | Drop, _ -> t2
  | _, Drop -> t1
  | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
    if compare f1 f2 = 0 then
      Union (f1, ValueMap.merge (fun v p1o p2o -> match p1o, p2o with
                                 | None, None -> None
                                 | Some p1, None -> Some (union_pair p1 d2)
                                 | None, Some p2 -> Some (union_pair d1 p2)
                                 | Some p1, Some p2 -> Some (union_pair p1 p2)) vm1 vm2, union_pair d1 d2)
    else if compare f1 f2 < 0 then
      Union (f1, ValueMap.map (fun p -> union_pair p t2) vm1, union_pair d1 t2)
    else
      union_pair t2 t1

let union = List.fold_left union_pair Drop

let rec seq_pair (t1: t) (t2: t) : t =
  match t1, t2 with
  | Drop, _
  | _, Drop -> Drop
  | Skip, _ -> t2
  | _, Skip -> t1
  | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
    if compare f1 f2 = 0 then
      Union (f1, ValueMap.merge (fun v p1o p2o -> match p1o, p2o with
                                 | None, None -> None
                                 | Some p1, None -> Some (seq_pair p1 d2)
                                 | None, Some p2 -> Some (seq_pair d1 p2)
                                 | Some p1, Some p2 -> Some (seq_pair p1 p2)) vm1 vm2, seq_pair d1 d2)
    else if compare f1 f2 < 0 then
      Union (f1, ValueMap.map (fun p -> seq_pair p t2) vm1, seq_pair d1 t2)
    else
      seq_pair t2 t1

let seq = List.fold_left seq_pair Skip

let intersect_pair = seq_pair
let intersect = seq

let star _ = Skip

let neg e = match e with
  | Skip -> Drop
  | Drop -> Skip
  | Union (f, vm, d) -> failwith "TODO"

let diff t1 t2 = intersect_pair t1 (neg t1)

let xor t1 t2 = union_pair (diff t1 t2) (diff t2 t1)

let rec to_exp = function
  | Skip -> Nkexp.skip
  | Drop -> Nkexp.drop
  | Union (f, vm, d) -> let tsts = Nkexp.union (List.map (fun (v,t') -> 
                            let tst = Nkexp.filter true f v in
                            let next = to_exp t' in
                            Nkexp.seq_pair tst next) (ValueMap.bindings vm)) in
                         let ntsts = Nkexp.seq (List.map (fun (v,_) ->
                            Nkexp.filter false f v) (ValueMap.bindings vm)) in
                         Nkexp.union_pair tsts (Nkexp.seq_pair ntsts (to_exp d))

let to_string t = to_exp t |> Nkexp.to_string
