
type t = Pk.t list

val empty : t
val pairs : t -> Pkpair.t list
val to_string : t -> string
