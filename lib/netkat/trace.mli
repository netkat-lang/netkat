(** Representation of traces (i.e. sequences) of packets. *)

type t = Pk.t list

module S : Set.S with type elt = t
module M : Map.S with type key = t

(** An empty trace with no packets. *)
val empty : t

(** Comparator for packet traces. *)
val compare : t -> t -> int

(** Equivalence relation for packet traces*)
val eq : t -> t -> bool

(** pairs [tr] converts a [Trace]-typed packet trace to a [PrTrace]-typed packet trace. [tr] must have at least two packets. *)
val pairs : t -> PrTrace.t

(** pairs [tr] converts a [Trace]-typed packet trace to a [PrTrace]-typed packet trace. *)
val of_pairs : PrTrace.t -> t

(** [to_string tr] is the string representation of the packet trace [tr]. *)
val to_string : t -> string

(** [prefixes tr] is the set of prefix traces of the trace [tr] that has at least two packets. [tr] must have size >= 2. *)
val prefixes : t -> t list

(** [suffixes tr] is the set of suffix traces of the trace [tr] that has at least two packets. [tr] must have size >= 2. *)
val suffixes : t -> t list

(** [suffixes tr] is the set of prefix traces of the trace [tr] that has at least one packets. [tr] must have size  >= 1. *)
val prefixes1 : t -> t list

(** [suffixes tr] is the set of suffix traces of the trace [tr] that has at least one packets. [tr] must have size  >= 1. *)
val suffixes1 : t -> t list

(** The first packet of a trace. *)
val hd : t -> Pk.t

(** The trace with the first packet removed. *)
val tl : t -> t

(** The last packet of a trace. *)
val dh : t -> Pk.t

(** The trace with the last packet removed.*)
val lt : t -> t

(** The length of a packet trace. *)
val length : t -> int
