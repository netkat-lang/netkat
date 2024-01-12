(** SPP implementation *)
open Pk

type t =
  | Skip 
  | Drop 
  | Union of (field * value * (value * t list)) list * t

let union_pair t1 t2 = failwith "TODO"
let union ts = failwith "TODO"

let seq_pair t1 t2 = failwith "TODO"
let seq ts = failwith "TODO"

let push (sp: Sp.t) (spp: t) = failwith "TODO"
let pull (spp: t) (sp: Sp.t) = failwith "TODO"
