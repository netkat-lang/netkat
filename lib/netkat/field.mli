(** NetKAT field values. This module contains a global-scope store to assign unique values to field names
as they are discovered. *)

type t

val compare : t -> t -> int

module M : Map.S with type key = t
module S : Set.S with type elt = t
val keys : 'a M.t -> S.t

(** [get_fields] returns a set of all the field names which have been created. *)
val get_fields : unit -> S.t

(** Lookup (or, generate a new) field for a given name. *)
val get_or_assign_fid : string -> t

(** Lookup a field by name, but if there is not one, fail instead of generating one. *)
val get_or_fail_fid : t -> string
