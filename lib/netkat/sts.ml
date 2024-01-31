module ExpMap = Map.Make(Nk)

type t = Spp.t ExpMap.t

let trans (sts: t) (e: Nk.t) : Spp.t = ExpMap.find e sts

let to_list = ExpMap.bindings

let drop = ExpMap.singleton Nk.drop Spp.skip
let dup = ExpMap.singleton Nk.skip Spp.skip

let union_pair t1 t2 = List.fold_left (fun m (e,spp) ->
  match ExpMap.find_opt e m with
  | None -> ExpMap.add e spp m
  | Some spp2 -> ExpMap.add e (Spp.union_pair spp spp2) m) t1 (ExpMap.bindings t2)

let union = List.fold_left union_pair drop

let intersect stss = failwith ("TODO" ^ __LOC__)
let intersect_pair t1 t2 = failwith ("TODO" ^ __LOC__)

let seq_spp spp sts = ExpMap.map (fun sppi -> Spp.seq_pair spp sppi) sts
let seq_exp sts exp = List.fold_left (fun m (e,spp) ->
  ExpMap.add (Nk.seq_pair e exp) spp m) ExpMap.empty (ExpMap.bindings sts)
