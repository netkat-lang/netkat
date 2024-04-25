(** SPP implementation *)
open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * ((t ValueMap.t) ValueMap.t) * (t ValueMap.t) * t

let skip = Skip
let drop = Drop

let map_op = map_op drop
let map_op_pair = map_op_pair drop
let right_join = right_join drop

let dummy f d = Union (f, ValueMap.empty, ValueMap.empty, d)

let rec compare spp1 spp2 = match spp1, spp2 with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union (f1,fms1,ms1,d1), Union (f2,fms2,ms2,d2) ->
      if f1 < f2 then -1
      else if f2 < f1 then 1
      else let cmp_fms = ValueMap.compare (ValueMap.compare compare) fms1 fms2 in
           if cmp_fms < 0 then -1
           else if cmp_fms > 0 then 1
           else let cmp_ms = ValueMap.compare compare ms1 ms2 in
                if cmp_ms < 0 then -1
                else if cmp_ms > 0 then 1
                else compare d1 d2

let eq spp1 spp2 = compare spp1 spp2 = 0

(** [mk_union (f, fms, ms, d)] performs canonicalization to guarantee that only
    semantically equivalent SPPs have the same representation. *)
let mk_union (f, fms, ms, d) = (*Union (f, fms, ms, d)*)
  (* First remove filter/mod branches that are exactly [Drop] *)
  let fms' = ValueMap.map (fun m -> ValueMap.filter_map (fun _ spp ->
    match spp with
        | Drop -> None
        | _ -> Some spp) m) fms in
  let fms'' = List.fold_left (fun m (vi, sppi) ->
    if eq sppi Drop && not (ValueMap.mem vi fms') then
      ValueMap.remove vi m else m) fms' (ValueMap.bindings ms) in
  let ms' = ValueMap.filter (fun _ sppi -> not (eq sppi Drop)) ms in
  let fms''' = ValueMap.filter (fun vi mi ->
    let drop_branch = if ValueMap.mem vi ms' || eq d Drop then
      ms' else ValueMap.add vi d ms' in
    not (ValueMap.equal eq mi drop_branch)) fms'' in
  if ValueMap.is_empty fms''' && ValueMap.is_empty ms' then
    d
  else
    Union (f, fms''', ms', d)
    
let rec of_sp = function
  | Sp.Skip -> Skip
  | Sp.Drop -> Drop
  | Sp.Union (f, vs, d) ->
      let vvs = List.fold_left (fun a (v, t) ->
        ValueMap.add v (ValueMap.singleton v (of_sp t)) a) ValueMap.empty (ValueMap.bindings vs) in
      mk_union (f, vvs, ValueMap.empty, of_sp d)


let rec to_sp_bwd (spp: t) : Sp.t = match spp with
  | Skip -> Sp.Skip
  | Drop -> Sp.Drop
  | Union (f, fms, ms, d) ->
      let ms1 = ValueMap.map (fun ms -> List.map (fun (_,sppi) -> (to_sp_bwd sppi))
                                        (ValueMap.bindings ms) |> Sp.union) fms in
      let y = List.map (fun (_,sppi) -> to_sp_bwd sppi) (ValueMap.bindings ms)
              |> Sp.union in
      let ms2 = List.fold_left (fun m (v, sppi) -> 
                                  match ValueMap.find_opt v fms with
                                  | None -> ValueMap.add v y m
                                  | Some _ -> m) ValueMap.empty (ValueMap.bindings ms) in
      Sp.mk_union (f, Pk.right_join Sp.Drop ms1 ms2, to_sp_bwd d)


let filter b f v =
  if b then
    mk_union (f, ValueMap.singleton v (ValueMap.singleton v Skip), ValueMap.empty, Drop)
  else
    mk_union (f, ValueMap.singleton v (ValueMap.empty), ValueMap.empty, Skip)

let modf f v = 
    mk_union (f, ValueMap.empty, ValueMap.singleton v Skip, Drop)

let rec to_exp = function
  | Skip -> Nk.skip
  | Drop -> Nk.drop
  | Union (f,fms,ms,d) ->
      let branches = ValueMap.bindings fms 
        |> List.map (fun (vi,m) ->
            let mods = ValueMap.bindings m
            |> List.map (fun (vj,spp) -> Nk.seq_pair (Nk.modif f vj) (to_exp spp))
            |> Nk.union in
            Nk.seq_pair (Nk.filter true f vi) mods)
        |> Nk.union in
      let uneq = ValueMap.bindings fms
        |> List.map (fun (vi,_) -> Nk.filter false f vi) |> Nk.seq in
      let mods = ValueMap.bindings ms
        |> List.map (fun (vi, sppi) -> Nk.seq_pair (Nk.modif f vi) (to_exp sppi))
        |> Nk.union in
      let def = Nk.seq_pair (ValueMap.bindings ms
        |> List.map (fun (vi,_) -> Nk.filter false f vi)
        |> Nk.seq) (to_exp d) in
      let defaults = Nk.seq_pair uneq (Nk.union_pair mods def) in
      Nk.union_pair branches defaults

let to_string t = to_exp t |> Nk.to_string

let vm_to_string (m: Sp.t ValueMap.t): string =
  List.map (fun (vj, sp) ->
      string_of_val vj ^ "↦" ^ (Sp.to_string sp)) (ValueMap.bindings m)
        |> String.concat ", "

let vmpp_to_string (m: t ValueMap.t): string =
  List.map (fun (vj, sp) ->
      string_of_val vj ^ "↦" ^ (to_string sp)) (ValueMap.bindings m)
        |> String.concat ", "

let vmm_to_string (m: t ValueMap.t ValueMap.t): string = 
  List.map (fun (vi, mi) ->
    (string_of_val vi) ^ "--->" ^ (vmpp_to_string mi)
    ) (ValueMap.bindings m) |> String.concat "; "

let get_branch (x: t) (v: value) : t ValueMap.t =
  match x with
  | Drop -> ValueMap.empty
  | Skip -> ValueMap.singleton v Skip
  | Union (f,fms,ms,d) -> match ValueMap.find_opt v fms with
                          | Some m -> m
                          | None -> if ValueMap.mem v ms ||  eq d Drop then
                                      ms
                                    else
                                      ValueMap.add v d ms

let rec union_pair spp1 spp2 = match spp1, spp2 with
  | Drop, _ -> spp2
  | _, Drop -> spp1
  | Skip, Skip -> Skip
  | Skip, Union (f,_,_,_) -> union_pair (dummy f Skip) spp2
  | Union (f,_,_,_), Skip -> union_pair spp1 (dummy f Skip)
  | Union (f1,fms1,ms1,d1), Union (f2,fms2,ms2,d2) ->
      if f1 < f2 then
        union_pair spp1 (dummy f1  spp2)
      else if f1 > f2 then
        union_pair (dummy f2 spp1) spp2
      else
        (* XXX As-yet unadapted section here:
          if (idL eq False) && (idR eq False) then
            if branchesL.isEmpty && branchesR.isEmpty then
              if mutsR.size == 1 && !mutsL.contains(mutsR.head._1) then
                // (1) If right is a single assignment only in the default test case
                return TestMut.mk(xL, branchesL, mutsL.updated(mutsR.head._1, mutsR.head._2), idL)
              else
                // (2) Any number of assignments, again only in the default test case
                return TestMut(xL, branchesL, unionMap(mutsL, mutsR), idL)
            if mutsL.isEmpty && branchesR.size == 1 && !branchesL.contains(branchesR.head._1) && mutsR.size == 0
                   // (3) Right side is only a single test?
              then return TestMut.mk(xL, branchesL.updated(branchesR.head._1, branchesR.head._2), mutsL, idL)
          *)
        let keyset = ValueSet.union (union_keys [fms1; fms2]) (union_keys [ms1; ms2]) in
        let fms = ValueSet.fold (fun v m ->
          let vm1 = get_branch spp1 v in
          let vm2 = get_branch spp2 v in
          let vm = map_op_pair union_pair vm1 vm2 in
          ValueMap.add v vm m) keyset ValueMap.empty in
        let ms = map_op_pair union_pair ms1 ms2 in
        let d = union_pair d1 d2 in
        mk_union (f1, fms, ms, d)

let union = List.fold_left union_pair Drop
let union_map_pair = map_op_pair union_pair
let union_maps = map_op union_pair

let rec seq_pair spp1 spp2 = match spp1, spp2 with
  | Skip, _ -> spp2
  | _, Skip -> spp1
  | Drop, _
  | _, Drop -> Drop
  | Union (f1,fms1,ms1,d1), Union (f2,fms2,ms2,d2) ->
      if f1 < f2 then
        seq_pair spp1 (dummy f1 spp2)
      else if f2 < f1 then
        seq_pair (dummy f2 spp1) spp2
      else
        let matchup (vi, sppi) =
          get_branch spp2 vi |> ValueMap.bindings
          |> List.fold_left (fun m (vj, sppj) ->
            ValueMap.add vj (seq_pair sppi sppj) m) ValueMap.empty in
        let msA = union_maps (List.map matchup (ValueMap.bindings ms1)) in
        let keyset = ValueSet.union (union_keys [fms1; fms2]) (union_keys [ms1; ms2; msA]) in
        let fms = ValueSet.fold (fun v m -> 
          let spp_v = get_branch spp1 v |> ValueMap.bindings in
          let res = List.map (fun (vi, sppi) ->
            let spp_v_i = get_branch spp2 vi |> ValueMap.bindings in
            List.fold_left (fun mi (vj, sppj) ->
              ValueMap.add vj (seq_pair sppi sppj) mi) ValueMap.empty spp_v_i) spp_v in
          ValueMap.add v (union_maps res) m) keyset ValueMap.empty in
        let msB = List.fold_left (fun m (v,spp) ->
          ValueMap.add v (seq_pair d1 spp) m) ValueMap.empty (ValueMap.bindings ms2) in
        mk_union (f1,
                  fms,
                  union_map_pair msA msB,
                  seq_pair d1 d2)

let seq = List.fold_left seq_pair Skip

let rec intersect_pair spp1 spp2 =
  match spp1,spp2 with
  | Drop, _
  | _, Drop -> Drop
  | Skip, Skip -> Skip
  | Skip, Union(f,fms,ms,d) -> intersect_pair (dummy f Skip) spp2
  | Union(f,fms,ms,d), Skip -> intersect_pair spp1 (dummy f Skip)
  | Union (f1,fms1,ms1,d1), Union (f2,fms2,ms2,d2) ->
      if f1 < f2 then
        intersect_pair spp1 (dummy f1 spp2)
      else if f2 < f1 then
        intersect_pair (dummy f2 spp1) spp2
      else
        let keyset = ValueSet.union (union_keys [fms1; fms2]) (union_keys [ms1; ms2]) in
        let fms = ValueSet.fold (fun v m -> 
          ValueMap.add v (map_op_pair intersect_pair (get_branch spp1 v) (get_branch spp2 v)) m) keyset ValueMap.empty in
        let ms = map_op_pair intersect_pair ms1 ms2 in
        let d = intersect_pair d1 d2 in
        mk_union(f1, fms, ms, d)

let intersect spps = match spps with
                     | [] -> Drop
                     | [x] -> x
                     | x::ys -> List.fold_left intersect_pair x ys

let rec diff spp1 spp2 = match spp1, spp2 with
  | Skip,Skip
  | Drop, _ -> Drop
  | _, Drop -> spp1
  | Skip, Union (f, fms, ms, d) -> diff (dummy f Skip) spp2
  | Union (f, fms, ms, d), Skip -> diff spp1 (dummy f Skip)
  | Union (f1, fms1, ms1, d1), Union (f2, fms2, ms2, d2) ->
      if f1 < f2 then diff spp1 (dummy f1 spp2) else
      if f2 < f1 then diff (dummy f2 spp1) spp2 else
        let keyset = ValueSet.union (union_keys [fms1; fms2]) (union_keys [ms1; ms2]) in
        let fms = ValueSet.fold (fun v m ->
          let vm = map_op_pair diff (get_branch spp1 v) (get_branch spp2 v) in
          ValueMap.add v vm m) keyset ValueMap.empty in
        let ms = map_op_pair diff ms1 ms2 in
        let d =  diff d1 d2 in
        mk_union (f1, fms, ms, d)

let xor spp1 spp2 = union_pair (diff spp1 spp2) (diff spp2 spp1)

(* TODO Optimize this somehow? *)
let rec star spp =
  let spp' = union [Skip; spp; seq_pair spp spp] in
  if eq spp spp' then
    spp
  else
    star spp'

let rec push (sp: Sp.t) (spp: t) = match sp, spp with
  | Sp.Drop, _
  | _, Drop -> Sp.Drop
  | _, Skip -> sp
  | Sp.Skip, Union (f, fms, ms, d) ->
      let thru_fms = List.map (fun (v,vms) ->
          ValueMap.mapi (fun v spp -> push Sp.Skip spp) vms) (ValueMap.bindings fms)
        |> Pk.map_op Sp.drop Sp.union_pair in
      let thru_ms = ValueMap.mapi (fun v spp -> push Sp.Skip spp) ms in
      let thru_both = Pk.map_op_pair Sp.drop Sp.union_pair thru_fms thru_ms in
      let matched_values = ValueSet.union (keys fms) (keys ms) in

      (* Next we need to identify the right slice of [Skip] to push through [d],
         i.e. whatever doesn't match [fms] or [ms]. *)
      let branchesD,unmatched_vs = ValueSet.fold (fun v (m,uvs) -> 
        match ValueMap.find_opt v thru_both with
        | None -> ValueMap.add v Sp.Drop m, v::uvs
        | Some _ -> m,uvs) matched_values (thru_both,[]) in
      let push_default = push Sp.Skip d in
      let diffkeys = ValueSet.diff (keys branchesD) matched_values
                 |> ValueSet.elements in
      let branchesE = List.fold_left (fun m v ->
          let sp_cur = ValueMap.find v branchesD in
          ValueMap.add v (Sp.union_pair sp_cur push_default) m) branchesD diffkeys in
      Sp.mk_union (f, branchesE, push_default)

  | Union (f1,fms1,d1), Union (f2,fms2,ms2,d2) ->
      if f1 < f2 then
        let fms = ValueMap.mapi (fun v spi -> push spi spp) fms1 in
        Sp.mk_union(f1, fms, push d1 spp)
      else if f2 < f1 then
        let fmsA = List.map (fun (_, muts) ->
          let tsts = ValueMap.mapi (fun v sppi -> push sp sppi) muts in
          Sp.mk_union(f2, tsts, Sp.Drop)) (ValueMap.bindings fms2) |> Sp.union in
        let ms = Pk.right_join Sp.drop (ValueMap.map (Fun.const Sp.drop) fms2)
                                       (ValueMap.map (fun sppi -> push sp sppi) ms2) in
        let fmsB = Sp.mk_union(f2, ms, push sp d2) in
        Sp.union_pair fmsA fmsB
      else  (* f1 = f2 *)
        (* XXX Do we need the comparison to drop case? if so it goes here. *)
        let pkA = List.map (fun (v,spi) ->
          match ValueMap.find_opt v fms2 with
          | None -> let tsts = ValueMap.map (fun sppj -> push spi sppj) ms2 in
                    begin
                    match ValueMap.find_opt v ms2 with
                    | None -> let tsts' = ValueMap.add v (push spi d2) tsts in
                              Sp.mk_union (f1, tsts', Sp.drop)
                    | Some sppj -> Sp.mk_union(f1, tsts, Sp.drop)
                    end
          | Some m -> let tsts = ValueMap.map (fun sppj -> push spi sppj) m in
                      Sp.mk_union(f1, tsts, Sp.drop)) (ValueMap.bindings fms1)
          |> Sp.union in
        let pkB = List.map (fun (v,sppi) -> if ValueMap.mem v fms1 then Sp.drop
                                            else let tsts = ValueMap.map (fun sppj -> push d1 sppj) sppi in
                                            Sp.mk_union(f1, tsts, Sp.drop)) (ValueMap.bindings fms2) |> Sp.union in
        let ms = ValueMap.map (Fun.const Sp.drop) fms1
                 |> Pk.left_join Sp.drop (ValueMap.map (Fun.const Sp.drop) fms2)
                 |> Pk.left_join Sp.drop (ValueMap.map (fun sppi -> push d1 sppi) ms2) in
        let pkC = Sp.mk_union(f1, ms, push d1 d2) in
        Sp.union [pkA; pkB; pkC]

let pull (spp: t) (sp: Sp.t) = seq_pair spp (of_sp sp) |> to_sp_bwd

let rep (spp: t) (fields: FieldSet.t) =
  let fresh s f m =
    let v = Pk.val_outside s in
    FieldMap.add f v m in
  let rec repr (s: t) (fs: FieldSet.t) (partial: (value*value) FieldMap.t) =
      match spp with
        | Drop -> failwith "Can't take representative of empty SPP!"
        | Skip -> let pk = FieldSet.fold (fresh ValueSet.empty) fields FieldMap.empty in (pk, pk)
        | Union (f, b, m, d) ->
            let mub = ValueSet.union (keys m) (keys b) in
            if d != Drop then repr d fs (fresh mub f partial) else

  repr spp fields FieldMap.empty
