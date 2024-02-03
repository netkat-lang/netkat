(** Represents a store for binding names to expressions and values *)

open Pk

type t

val empty : t

val bind_exp : t -> string -> Nk.t -> t
val lookup_exp : t -> string -> Nk.t

val bind_val : t -> string -> value -> t
val lookup_val : t -> string -> value
