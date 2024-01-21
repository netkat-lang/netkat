

type t

val trans : t -> Nkexp.t -> Spp.t

val to_list : t -> (Nkexp.t * Spp.t) list

val skip : t
val drop : t

val union : t list -> t
val union_pair : t -> t -> t

val intersect : t list -> t
val intersect_pair : t -> t -> t
