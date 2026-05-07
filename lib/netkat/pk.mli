(** Representation of individual (i.e., concrete) packets in NetKAT. *)

type field = Field.t

type value = Value.t

type t = value Field.M.t

val add : t -> field -> value -> t

val singleton : field -> value -> t

val empty : t

(** comparator of packets. *)
val compare : t -> t -> int

(** equivalence relation of packets. *)
val eq : t -> t -> bool

(** string representation of packets. *)
val to_string : t -> string
