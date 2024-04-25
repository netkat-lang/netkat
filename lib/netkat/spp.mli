(** SPP *)

open Pk

type t

val skip : t
val drop : t

val filter : bool -> field -> value -> t
val modf : field -> value -> t

val to_exp : t -> Nk.t
val to_string : t -> string

val of_sp : Sp.t -> t

val compare : t -> t -> int
val eq : t -> t -> bool

val union_pair : t -> t -> t
val union : t list -> t

val seq_pair : t -> t -> t
val seq : t list -> t

val intersect_pair : t -> t -> t
val intersect : t list -> t

val xor : t -> t -> t
val diff : t -> t -> t

val star : t -> t

val push : Sp.t -> t -> Sp.t
val pull : t -> Sp.t -> Sp.t

val rep : t -> FieldSet.t -> Pk.t * Pk.t
