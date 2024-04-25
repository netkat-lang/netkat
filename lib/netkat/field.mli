type t

val compare : t -> t -> int

module M : Map.S with type key = t
module S : Set.S with type elt = t
val keys : 'a M.t -> S.t

(** Lookup (or, generate a new) int label for a field name *)
val get_or_assign_fid : string -> t
val get_or_fail_fid : t -> string
