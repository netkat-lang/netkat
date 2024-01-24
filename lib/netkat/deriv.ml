
(* --- Brzozowski derivatives --- *)

let rec e = function
  | Nkexp.Drop -> Spp.Drop
  | Nkexp.Skip -> Spp.Skip
  | Nkexp.Dup -> Spp.Drop
  | Nkexp.Filter (b, f, v) -> Spp.filter b f v
  | Nkexp.Mod (f, v) -> Spp.modf f v
  | Nkexp.Seq es -> List.map e es |> Spp.seq
  | Nkexp.Union es -> List.map e es |> Spp.union
  | Nkexp.Star _ -> Spp.Skip
  | Nkexp.Intersect es -> List.map e es |> Spp.intersect
  | Nkexp.Neg exp -> failwith "Derivative is not defined for negation of arbitrary expressions"
  | Nkexp.Fwd _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Bwd _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Exists _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Forall _ -> failwith ("TODO: " ^ __LOC__)
  


let rec d = function
  | Nkexp.Drop
  | Nkexp.Skip
  | Nkexp.Filter _
  | Nkexp.Mod _ -> Sts.drop
  | Nkexp.Dup -> Sts.dup
  | Nkexp.Seq es -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Union es -> List.map d es |> Sts.union
  | Nkexp.Star _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Intersect es -> List.map d es |> Sts.intersect
  | Nkexp.Neg exp -> failwith "Derivative is not defined for negation of arbitrary expressions"
  | Nkexp.Fwd _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Bwd _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Exists _ -> failwith ("TODO: " ^ __LOC__)
  | Nkexp.Forall _ -> failwith ("TODO: " ^ __LOC__)
