(** SP *)

open Pk

type t =
  | Skip 
  | Drop 
  | Union of field * (t ValueMap.t) * t

val compare : t -> t -> int
val eq : t -> t -> bool

val le : t -> t -> bool

val skip : t
val drop : t

val union_pair : t -> t -> t
val union : t list -> t

val seq_pair : t -> t -> t
val seq : t list -> t

val intersect_pair : t -> t -> t
val intersect : t list -> t

val star : t -> t

val diff : t -> t -> t
val neg : t -> t

val xor : t -> t -> t

(*---------- Operations for computing push/pull:---------- *)

val union_maps : t ValueMap.t list -> t ValueMap.t
val union_map_pair : t ValueMap.t -> t ValueMap.t -> t ValueMap.t

(*---------- Output ------------------ *)
val to_exp : t -> Nkexp.t
val to_string : t -> string
