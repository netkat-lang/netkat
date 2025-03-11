(** Operations involving packet values. *)

type t

val compare : t -> t -> int

val of_int : int -> t
val to_string : t -> string

module M : sig
  include Map.S with type key = t
  val fold_bdgs : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
end
module S : Set.S with type elt = t

(*---------- Utility operations for ValueMaps---------------------- *)

val map_op_pair : 'a -> ('a -> 'a -> 'a) -> 'a M.t -> 'a M.t -> 'a M.t
val map_op : 'a -> ('a -> 'a -> 'a) -> 'a M.t list -> 'a M.t
val right_join : 'a -> 'a M.t -> 'a M.t -> 'a M.t
val left_join : 'a -> 'a M.t -> 'a M.t -> 'a M.t

val keys : 'a M.t -> S.t
val union_keys : 'a M.t list -> S.t
val val_outside : S.t -> t
val choose : t
