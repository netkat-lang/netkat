(** SPP implementation *)
open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * ((t ValueMap.t) ValueMap.t) * (t ValueMap.t) * t

let rec of_sp = function
  | Sp.Skip -> Skip
  | Sp.Drop -> Drop
  | Sp.Union (f, vs, d) ->
      let vvs = List.fold_left (fun a (v, t) ->
        ValueMap.add v (ValueMap.singleton v (of_sp t)) a) ValueMap.empty (ValueMap.bindings vs) in
      Union (f, vvs, ValueMap.empty, of_sp d)

let filter b f v =
  if b then
    Union (f, ValueMap.singleton v (ValueMap.singleton v Skip), ValueMap.empty, Drop)
  else
    Union (f, ValueMap.singleton v (ValueMap.singleton v Drop), ValueMap.empty, Skip)

let modf f v = 
    Union (f, ValueMap.empty, ValueMap.singleton v Skip, Drop)

let to_exp = function
  | Skip -> Nkexp.skip
  | Drop -> Nkexp.drop
  | Union _ -> failwith ("TODO: " ^ __LOC__)

let to_string t = to_exp t |> Nkexp.to_string

let compare spp1 spp2 = match spp1, spp2 with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)

let eq spp1 spp2 = compare spp1 spp2 = 0

let union_pair spp1 spp2 = match spp1, spp2 with
  | Skip, _
  | _, Skip -> Skip
  | Drop, _ -> spp2
  | _, Drop -> spp1
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)

let union spps = failwith ("TODO: " ^ __LOC__)

let seq_pair spp1 spp2 = failwith ("TODO: " ^ __LOC__)
let seq spps = failwith ("TODO: " ^ __LOC__)

let intersect_pair spp1 spp2 = match spp1,spp2 with
  | Drop, _
  | _, Drop -> Drop
  | Skip, _ -> spp2
  | _, Skip -> spp1
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)
  
let intersect _ = failwith ("TODO: " ^ __LOC__)
let diff spp1 spp2 = match spp1, spp2 with
  | Drop, _
  | _, Skip -> Drop
  | _, Drop -> spp1
  | Skip, _ -> failwith ("TODO: " ^ __LOC__)
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)

let xor spp1 spp2 = union_pair (diff spp1 spp2) (diff spp2 spp1)

let star spp = failwith ("TODO: " ^ __LOC__)

let rec push (sp: Sp.t) (spp: t) = match sp, spp with
  | Sp.Drop, _
  | _, Drop -> Sp.Drop
  | _, Skip -> sp
  | Sp.Skip, Union (f, fms, ms, d) ->
      let branchesA = List.map (fun (v,vms) ->
          ValueMap.mapi (fun v spp -> push Sp.Skip spp) vms) (ValueMap.bindings fms)
        |> Sp.union_maps in
      let branchesB = ValueMap.mapi (fun v spp -> push Sp.Skip spp) ms in
      let branchesC = Sp.union_map_pair branchesA branchesB in
      let matched_values = ValueMap.((List.map (fun (v,_) -> v) (bindings fms))
                                    @(List.map (fun (v,_) -> v) (bindings ms))) in
      let branchesD,unmatched_vs = List.fold_left (fun (m,uvs) v -> 
        match ValueMap.find_opt v branchesC with
        | None -> ValueMap.add v Sp.Drop m, v::uvs
        | Some _ -> m,uvs) (branchesC,[]) matched_values in
      (* XXX: still not quite matching scala here... *) 
      (*
      let branchesE = ValueMap.mapi
        ValueMap.add v (Sp.union_pair ) branchesC m
        *)
      let push_default = push Sp.Skip d in
        (* There is more code here in Jules' version I don't understand yet. *)
      Sp.Union (f, branchesD, push_default)
  | Union (f1,fms1,d1), Union (f2,fms2,ms2,d2) ->
      Sp.Drop (*XXX*)


let pull (spp: t) (sp: Sp.t) = match spp with
  | Drop -> Sp.drop
  | Skip -> sp
  | Union _ -> failwith ("TODO: " ^ __LOC__)
