(** SPP implementation *)
open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * ((t ValueMap.t) ValueMap.t) * (t ValueMap.t) * t

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

let push (sp: Sp.t) (spp: t) = match sp, spp with
  | Sp.Drop, _
  | _, Drop -> Sp.Drop
  | _, Skip -> sp
  | Sp.Skip, Union _ -> failwith ("TODO: " ^ __LOC__)
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)

let pull (spp: t) (sp: Sp.t) = match spp with
  | Drop -> Sp.drop
  | Skip -> sp
  | Union _ -> failwith ("TODO: " ^ __LOC__)
