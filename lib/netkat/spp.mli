(** SPP *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of (field * value * (value * t list)) list * t

val union_pair : t -> t -> t
val union : t list -> t

val seq_pair : t -> t -> t
val seq : t list -> t

val push : Sp.t -> t -> Sp.t
val pull : t -> Sp.t -> Sp.t
