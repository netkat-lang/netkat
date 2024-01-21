(** SPP *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * ((t ValueMap.t) ValueMap.t) * (t ValueMap.t) * t

val compare : t -> t -> int
val eq : t -> t -> bool

val union_pair : t -> t -> t
val union : t list -> t

val seq_pair : t -> t -> t
val seq : t list -> t

val intersect : t list -> t

val xor : t -> t -> t
val diff : t -> t -> t

val star : t -> t

val push : Sp.t -> t -> Sp.t
val pull : t -> Sp.t -> Sp.t
