(** Operations involving packet values. *)
type t

module SMap : Map.S with type key = string

module Env : sig
  type t = int SMap.t
  val empty : t
  val add : string -> int -> t -> t
  val singleton : string -> int -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

module EnvMap : sig
  include Map.S with type key = Env.t
end

module IntSet : sig
  include Set.S with type elt = int
end

val collecting_assignments : bool ref
val temp_assignments : (string,IntSet.t) Hashtbl.t
val get_temp_bindings : unit -> ((string * int) list) list

val print_assignments : unit -> unit

val start_collecting : Env.t -> unit
val stop_collecting : unit -> unit

module CustomInt : sig
  type t
  val compare_env : bool -> Env.t -> t -> t -> int EnvMap.t
  val to_string : t -> string
end

(** Comparator for packet values. *)
val compare : t -> t -> int

(** [of_int x] is the packet value corresponding to the integer [x]. *)
val of_int : int -> t

(** [to_string v] is the string representation of [v]. *)
val to_string : t -> string

(** [of_string s] is the field representation of [s]. *)
val of_string : string -> t

(** Map type for packet values. *)
module M : sig
  include Map.S with type key = t
  val fold_bdgs : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val compare_env : Env.t -> (Env.t -> 'a -> 'a -> int EnvMap.t) -> 'a t -> 'a t -> int EnvMap.t
end

(** Set type for packet values. *)
module S : Set.S with type elt = t

(*---------- Utility operations for ValueMaps---------------------- *)

(** [map_op_pair d f m1 m2] is the map [m] containing any key [k] from either [m1] or [m2] such that: ([M.find m k] is abbreviated to [m[k]] for brevity):

{ul
  {- [m[k] = f d m2[k]] if [k] does not exist in [m1], }
  {- [m[k] = f m1[k] d] if [k] does not exist in [m2],}
  {- [m[k] = f m1[k] m2[k]] if otherwise.}} *)
val map_op_pair : 'a -> ('a -> 'a -> 'a) -> 'a M.t -> 'a M.t -> 'a M.t

(** [map_op d f ms] iteratively performs [map_op_pair d f mi mj] on the list of maps [ms]. *)
val map_op : 'a -> ('a -> 'a -> 'a) -> 'a M.t list -> 'a M.t

(** [right_join d m1 m2] is [map_op_pair d (fun a b -> b) m1 m2]. [right_join d m1 m2] computes a conventional right join if [d = None]. *)
val right_join : 'a -> 'a M.t -> 'a M.t -> 'a M.t

(** [left_join d m1 m2] is [map_op_pair d (fun a b -> a) m1 m2]. [left_join d m1 m2] computes a conventional left join if [d = None]. *)
val left_join : 'a -> 'a M.t -> 'a M.t -> 'a M.t

(** The set of keys of a map. *)
val keys : 'a M.t -> S.t

(** The set of all keys of a list of maps. *)
val union_keys : 'a M.t list -> S.t

(** [val_outside s] is a packet value that is not in the set [s] of packet values.*)
val val_outside : S.t -> t

(** [choose] is some packet value.*)
val choose : t
