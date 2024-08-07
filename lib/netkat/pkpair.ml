open Pk

type t = (value*value) Field.M.t

let compare = Field.M.compare (fun (v1,v2) (v1',v2') -> 
  let cmp = Value.compare v1 v1' in
  if cmp = 0 then
    Value.compare v2 v2'
  else
    cmp)

let eq pp pp' = compare pp pp' = 0

let split (t: t) : Pk.t * Pk.t =
  let splitf f (v1,v2) (p1,p2) = (Field.M.add f v1 p1, Field.M.add f v2 p2) in
  Field.M.fold splitf t (Field.M.empty, Field.M.empty)

let mk (t1: Pk.t) (t2: Pk.t) : t =
  let mkf f pp = let v1 = Field.M.find f t1 in
                  let v2 = Field.M.find f t2 in
                  Field.M.add f (v1,v2) pp in
  Field.S.fold mkf (Field.keys t1) Field.M.empty


let getf_opt (t: t) (f: field) = Field.M.find_opt f t
let addf = Field.M.add
let empty = Field.M.empty

let to_string pp =
  let (a,b) = split pp in
  (Pk.to_string a) ^ "->" ^ (Pk.to_string b)

let to_list = Field.M.bindings
