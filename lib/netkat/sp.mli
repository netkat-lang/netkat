(** SP *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * (t ValueMap.t) * t

val skip : t
val drop : t

val union_pair : t -> t -> t
val seq_pair : t -> t -> t

val to_exp : t -> Nkexp.t
val to_string : t -> string
