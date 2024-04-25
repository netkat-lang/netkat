open Pk

type t = (value*value) FieldMap.t

let split (t: t) : Pk.t * Pk.t =
  let splitf f (v1,v2) (p1,p2) = (FieldMap.add f v1 p1, FieldMap.add f v2 p2) in
  FieldMap.fold splitf t (FieldMap.empty, FieldMap.empty)

let zip (t1: Pk.t) (t2: Pk.t) : t =
  let zipf f pp = let v1 = FieldMap.find f t1 in
                  let v2 = FieldMap.find f t2 in
                  FieldMap.add f (v1,v2) pp in
  FieldSet.fold zipf (Field.keys t1) FieldMap.empty
