
type t = Pk.t list

module S : Set.S with type elt = t
module M : Map.S with type key = t

val empty : t
val pairs : t -> Pkpair.t list
val to_string : t -> string
val prefixes : t -> S.t
val suffixes : t -> S.t
