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
      failwith "TODO"
    else if compare f1 f2 < 0 then
      failwith "TODO"
    else
      union_pair t2 t1

let (*rec*) seq_pair (t1: t) (t2: t) : t = failwith "TODO"

let rec to_exp = function
  | Skip -> Nkexp.skip
  | Drop -> Nkexp.drop
  | Union (f, vm, d) -> let tsts = Nkexp.union (List.map (fun (v,t') -> 
                            Nkexp.seq_pair (Nkexp.filter true f v) (to_exp t')) (ValueMap.bindings vm)) in
                         let ntsts = Nkexp.seq (List.map (fun (v,_) -> Nkexp.filter false f v) (ValueMap.bindings vm)) in
                         Nkexp.union_pair tsts (Nkexp.seq_pair ntsts (to_exp d))

let to_string t = to_exp t |> Nkexp.to_string
