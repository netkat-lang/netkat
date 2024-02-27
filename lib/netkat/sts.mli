type t

val trans : t -> Nk.t -> Spp.t

val to_list : t -> (Nk.t * Spp.t) list

val drop : t
val dup : t

val union : t list -> t
val union_pair : t -> t -> t

val intersect_pair : t -> t -> t
val intersect : t list -> t

val seq_spp : Spp.t -> t -> t
val seq_exp : t -> Nk.t -> t

val diff : t -> t -> t

val to_exp : t -> Nk.t
val to_string : t -> string

