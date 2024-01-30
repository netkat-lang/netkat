

type t

val trans : t -> Nkexp.t -> Spp.t

val to_list : t -> (Nkexp.t * Spp.t) list

val drop : t
val dup : t

val union : t list -> t
val union_pair : t -> t -> t

val intersect : t list -> t
val intersect_pair : t -> t -> t

val seq_spp : Spp.t -> t -> t
val seq_exp : t -> Nkexp.t -> t
