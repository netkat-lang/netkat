(** SPP implementation *)
open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * ((t ValueMap.t) ValueMap.t) * (t ValueMap.t) * t

let compare spp1 spp2 = match spp1, spp2 with
  | Drop, Drop -> 0
  | Drop, _ -> -1
  | _, Drop -> 1
  | Skip, Skip -> 0
  | Skip, _ -> -1
  | _, Skip -> 1
  | Union _, Union _ -> failwith ("TODO: " ^ __LOC__)

let eq spp1 spp2 = compare spp1 spp2 = 0

let union_pair spp1 spp2 = failwith ("TODO: " ^ __LOC__)
let union spps = failwith ("TODO: " ^ __LOC__)

let seq_pair spp1 spp2 = failwith ("TODO: " ^ __LOC__)
let seq spps = failwith ("TODO: " ^ __LOC__)

let intersect _ = failwith ("TODO: " ^ __LOC__)
let diff _ _ = failwith ("TODO: " ^ __LOC__)
let xor _ _ = failwith ("TODO: " ^ __LOC__)

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
