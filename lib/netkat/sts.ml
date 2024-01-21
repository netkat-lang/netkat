module ExpMap = Map.Make(Nkexp)

type t = Spp.t ExpMap.t

let trans (sts: t) (e: Nkexp.t) : Spp.t = ExpMap.find e sts

let to_list = ExpMap.bindings

let skip = ExpMap.singleton Nkexp.skip Spp.Skip
let drop = ExpMap.singleton Nkexp.drop Spp.Drop

let union stss = failwith ("TODO" ^ __LOC__)
let union_pair t1 t2 = failwith ("TODO" ^ __LOC__)

let intersect stss = failwith ("TODO" ^ __LOC__)
let intersect_pair t1 t2 = failwith ("TODO" ^ __LOC__)
