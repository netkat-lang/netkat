(** For packet related operations *)

type field
type value

val cmp_field : field -> field -> int
val cmp_value : value -> value -> int

module FieldMap : Map.S with type key = field
module ValueMap : sig
  include Map.S with type key = value
  val fold_bdgs : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
end
module ValueSet : Set.S with type elt = value

(** Lookup (or, generate a new) int label for a field name *)
val get_or_assign_fid : string -> field
val get_or_fail_fid : field -> string

(** Convert int to value. *)
val value_of_int : int -> value
val string_of_val : value -> string

(*---------- Utility operations for ValueMaps---------------------- *)

val map_op_pair : 'a -> ('a -> 'a -> 'a) -> 'a ValueMap.t -> 'a ValueMap.t -> 'a ValueMap.t
val map_op : 'a -> ('a -> 'a -> 'a) -> 'a ValueMap.t list -> 'a ValueMap.t
val right_join : 'a -> 'a ValueMap.t -> 'a ValueMap.t -> 'a ValueMap.t
val left_join : 'a -> 'a ValueMap.t -> 'a ValueMap.t -> 'a ValueMap.t

val keys : 'a ValueMap.t -> ValueSet.t
val union_keys : 'a ValueMap.t list -> ValueSet.t


(* Operations for concrete packets *)

type t
