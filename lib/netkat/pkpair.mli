open Pk

type t

val compare : t -> t -> int
val eq : t -> t -> bool

val split : t -> Pk.t * Pk.t
val mk : Pk.t -> Pk.t -> t
val addf : field -> value * value -> t -> t
val getf_opt : t -> field -> (value * value) option
val empty : t

val to_string : t -> string
val to_list : t -> (field*(value*value)) list
