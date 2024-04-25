(** SP implementation *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * (t Value.M.t) * t

let skip = Skip
let drop = Drop

let rec compare sp1 sp2 = match sp1, sp2 with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union (f1, fs1, d1), Union (f2, fs2, d2) ->
      if f1 < f2 then -1
      else if f2 < f1 then 1
      else
        let cmp_maps = Value.M.compare compare fs1 fs2 in
        if cmp_maps = 0 then
          compare d1 d2
        else
          cmp_maps

let eq sp1 sp2 = compare sp1 sp2 = 0

let mk_union (f, m, d) =
  let m' = List.fold_left (fun m (vi,spi) ->
    if eq spi d then
      m
    else
      Value.M.add vi spi m) Value.M.empty (Value.M.bindings m) in
  if Value.M.cardinal m' = 0 then
    d
  else
    Union (f, m', d)

let rec union_pair (t1:t) (t2:t) : t =
  match t1,t2 with
  | Skip, _
  | _, Skip -> Skip
  | Drop, _ -> t2
  | _, Drop -> t1
  | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
    if Field.compare f1 f2 = 0 then
      mk_union (f1, Value.M.merge (fun v p1o p2o -> match p1o, p2o with
                                 | None, None -> None
                                 | Some p1, None -> Some (union_pair p1 d2)
                                 | None, Some p2 -> Some (union_pair d1 p2)
                                 | Some p1, Some p2 -> Some (union_pair p1 p2)) vm1 vm2, union_pair d1 d2)
    else if Field.compare f1 f2 < 0 then
      mk_union (f1, Value.M.map (fun p -> union_pair p t2) vm1, union_pair d1 t2)
    else
      union_pair t2 t1

let union = List.fold_left union_pair Drop

let le sp1 sp2 = match sp1, sp2 with
  | Drop, _
  | _, Skip -> true
  | _, Drop
  | Skip, _ -> false (* because _, Skip is already marked true *)
  | _, _ -> eq sp2 (union_pair sp1 sp2)


let rec seq_pair (t1: t) (t2: t) : t =
  match t1, t2 with
  | Drop, _
  | _, Drop -> Drop
  | Skip, _ -> t2
  | _, Skip -> t1
  | Union (f1, vm1, d1), Union (f2, vm2, d2) ->
    if Field.compare f1 f2 = 0 then
      mk_union (f1, Value.M.merge (fun v p1o p2o -> match p1o, p2o with
                                 | None, None -> None
                                 | Some p1, None -> Some (seq_pair p1 d2)
                                 | None, Some p2 -> Some (seq_pair d1 p2)
                                 | Some p1, Some p2 -> Some (seq_pair p1 p2)) vm1 vm2, seq_pair d1 d2)
    else if Field.compare f1 f2 < 0 then
      mk_union (f1, Value.M.map (fun p -> seq_pair p t2) vm1, seq_pair d1 t2)
    else
      seq_pair t2 t1

let seq = List.fold_left seq_pair Skip

let intersect_pair = seq_pair
let intersect = seq

let star _ = Skip

let rec neg e = match e with
  | Skip -> Drop
  | Drop -> Skip
  | Union (f, vm, d) -> mk_union (f, Value.M.map neg vm, neg d)

let diff t1 t2 = intersect_pair t1 (neg t2)

let xor t1 t2 = union_pair (diff t1 t2) (diff t2 t1)

let rec to_exp = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f, vm, d) -> let tsts = Nk.union (List.map (fun (v,t') -> 
                            let tst = Nk.filter true f v in
                            let next = to_exp t' in
                            Nk.seq_pair tst next) (Value.M.bindings vm)) in
                         let ntsts = Nk.seq (List.map (fun (v,_) ->
                            Nk.filter false f v) (Value.M.bindings vm)) in
                         Nk.union_pair tsts (Nk.seq_pair ntsts (to_exp d))

let to_string t = to_exp t |> Nk.to_string
