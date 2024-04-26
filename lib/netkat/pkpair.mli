open Pk

type t

val split : t -> Pk.t * Pk.t
val mk : Pk.t -> Pk.t -> t
val addf : field -> value * value -> t -> t
val empty : t
