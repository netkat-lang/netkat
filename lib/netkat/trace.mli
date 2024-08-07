
type t = Pk.t list

module S : Set.S with type elt = t
module M : Map.S with type key = t

val empty : t
val compare : t -> t -> int
val eq : t -> t -> bool
val pairs : t -> PrTrace.t
val of_pairs : PrTrace.t -> t
(* val pairs_and_pres : t -> (Pkpair.t * t) list *)
val to_string : t -> string
val prefixes : t -> t list
val suffixes : t -> t list
val prefixes1 : t -> t list
val suffixes1 : t -> t list

val hd : t -> Pk.t
val tl : t -> t
val dh : t -> Pk.t
val lt : t -> t

val length : t -> int
