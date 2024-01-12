(** For packet related operations *)

type field
type value

val cmp_field : field -> field -> int
val cmp_value : value -> value -> int

module FieldMap : Map.S with type key = field
module ValueMap : Map.S with type key = value

(** Lookup (or, generate a new) int label for a field name *)
val get_or_assign_fid : string -> field
val get_or_fail_fid : field -> string

(** Convert int to value. *)
val value_of_int : int -> value
val string_of_val : value -> string
