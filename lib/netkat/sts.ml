module ExpMap = Map.Make(Nk)

type t = Spp.t ExpMap.t

let trans (e: Nk.t) (sts: t) : Spp.t = match ExpMap.find_opt e sts with
                                       | None -> Spp.drop
                                       | Some spp -> spp

let to_list = ExpMap.bindings

let drop = ExpMap.singleton Nk.drop Spp.skip
let dup = ExpMap.singleton Nk.skip Spp.skip

let add_nonempty e s m =
  if Spp.eq s Spp.drop then m else ExpMap.add e (Spp.union_pair (trans e m) s) m

(** Add a transition to [e] along [spp]. The trick is that SPPs
    transitions out of [t] need to be disjoint, so we have to go through and
    update other transitions to maintain the invariant. *)
let add (t: t) (e: Nk.t) (spp: Spp.t) =
  if Nk.eq e Nk.drop then t else
  List.fold_left (fun (m,s) (ei,sppi) ->
    let inter = Spp.intersect_pair s sppi in
    let diff = Spp.diff sppi s in
    let s' = Spp.diff s sppi in
    add_nonempty (Nk.union_pair e ei) inter m
    |> add_nonempty ei diff, s') (ExpMap.empty, spp) (ExpMap.bindings t)
   |> fun (r, s_rem) -> add_nonempty e s_rem r

let union_pair t1 t2 = List.fold_left (fun t (e,spp) ->
  add t e spp) t1 (ExpMap.bindings t2)

let union = List.fold_left union_pair drop

let intersect_pair t1 t2 = List.fold_left (fun m (qi,pi) -> 
  List.fold_left (fun m' (qj,pj) ->
    let qcap = Nk.intersect_pair qi qj in
    let pcap = Spp.intersect_pair pi pj in
    let pnew = match ExpMap.find_opt qcap m' with
               | None -> pcap
               | Some p -> Spp.union_pair p pcap in
    ExpMap.add qcap pnew m') m (ExpMap.bindings t2)) ExpMap.empty (to_list t1)

let intersect stss = match stss with
                     | [] -> drop
                     | [x] -> x
                     | x::es -> List.fold_left intersect_pair x es

let seq_spp spp sts = List.fold_left (fun t (e,sppi) ->
      add t e (Spp.seq_pair spp sppi)) ExpMap.empty (to_list sts)

let seq_exp sts exp = List.fold_left (fun t (e,sppi) ->
      add t (Nk.seq_pair e exp) sppi) ExpMap.empty (to_list sts)

let to_exp t = List.map (fun (e,spp) -> Nk.seq [Spp.to_exp spp; Nk.dup; e]) (to_list t) |> Nk.union

let diff t1 t2 =
  let d = List.fold_left (fun m (qi,pi) -> 
            List.fold_left (fun m' (qj,pj) ->
              let pcap = Spp.intersect_pair pi pj in
              let qdiff = Nk.diff qi qj in
              add m' qdiff pcap) m (to_list t2)) ExpMap.empty (to_list t1) in
  let all2 = to_list t2 |> List.map (fun (e,spp) -> spp) |> List.fold_left Spp.union_pair Spp.drop in
  List.fold_left (fun m (e, spp) -> add m e (Spp.diff spp all2)) d (to_list t1)

let xor t1 t2 = union_pair (diff t1 t2) (diff t2 t1)

let to_string t = List.map (fun (e,spp) -> "--[" ^ (Spp.to_string spp) ^ "]-->" ^ (Nk.to_string e)) (to_list t)
                     |> String.concat "; "
