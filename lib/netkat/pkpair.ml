open Pk

type t = (value*value) Field.M.t

let split (t: t) : Pk.t * Pk.t =
  let splitf f (v1,v2) (p1,p2) = (Field.M.add f v1 p1, Field.M.add f v2 p2) in
  Field.M.fold splitf t (Field.M.empty, Field.M.empty)

let zip (t1: Pk.t) (t2: Pk.t) : t =
  let zipf f pp = let v1 = Field.M.find f t1 in
                  let v2 = Field.M.find f t2 in
                  Field.M.add f (v1,v2) pp in
  Field.S.fold zipf (Field.keys t1) Field.M.empty
