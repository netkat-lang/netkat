(** Representation of individual (i.e., concrete) packets in NetKAT. *)

type field = Field.t
type value = Value.t

type t = value Field.M.t

val compare : t -> t -> int
val eq : t -> t -> bool

val to_string : t -> string
