(** Representation of a pair of individual (i.e., concrete) packets in NetKAT. *)

open Pk

type t

(** comparator for packet pairs. *)
val compare : t -> t -> int

(** equivalence relation for packet pairs. *)
val eq : t -> t -> bool

(** [split p] is [pk1, pk2] if [p] contains the two packets [pk1, pk2]. *)
val split : t -> Pk.t * Pk.t

(** [mk pk1 pk2] is the packet pair containing [pk1] and [pk2]. *)
val mk : Pk.t -> Pk.t -> t

(** [addf f (v1, v2) p] assigns the value [v1] to field [f] of the first packet and [v2] to the second packet in [p]. *)
val addf : field -> value * value -> t -> t

(** [getf_opt p f] is [Some (v1, v2)] if both packets has the field [f] and [v1] is the value of [f] in the first packet and [v2] the second, and [getf_opt p f] is [None (v1, v2)] if otherwise. *)
val getf_opt : t -> field -> (value * value) option

(** [empty] is a packet pair with two empty packets. *)
val empty : t

(** A string representation of pair of packets. *)
val to_string : t -> string

(** [to_list p] is a list of tuples of form [(f, (vf1, vf2))] where [vf1] is the value of the field [f] in the first packet and [vf2] the second. *)
val to_list : t -> (field*(value*value)) list
