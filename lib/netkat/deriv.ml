include Nk

(* --- Brzozowski derivatives --- *)

let rec e = function
  | Nk.Drop -> Spp.drop
  | Nk.Skip -> Spp.skip
  | Nk.Dup -> Spp.drop
  | Nk.Filter (b, f, v) -> Spp.filter b f v
  | Nk.Mod (f, v) -> Spp.modf f v
  | Nk.Seq es -> List.map e es |> Spp.seq
  | Nk.Union es -> List.map e es |> Spp.union
  | Nk.Star exp -> e exp |> Spp.star
  | Nk.Intersect es -> List.map e es |> Spp.intersect
  | Nk.Diff (e1,e2) -> Spp.diff (e e1) (e e2)

let rec d = function
  | Nk.Drop
  | Nk.Skip
  | Nk.Filter _
  | Nk.Mod _ -> Sts.drop
  | Nk.Dup -> Sts.dup
  | Nk.Seq es -> begin 
    match es with
    | [] -> Sts.drop
    | exp::res -> Sts.union_pair (Sts.seq_spp (e exp) (d (Nk.seq res)))
                                 (Sts.seq_exp (d exp) (Nk.seq res))
    end
  | Nk.Union es -> List.map d es |> Sts.union
  | Nk.Star exp -> Sts.seq_spp (Spp.star (e exp))
                               (Sts.seq_exp (d exp) (Nk.star exp))
  | Nk.Intersect es -> List.map d es |> Sts.intersect
  | Nk.Diff (e1,e2) -> Sts.diff (d e1) (d e2)
