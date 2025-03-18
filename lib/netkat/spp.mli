(** Representation of a Symbolic Packet Program (SPP). Hash-consing is used in this module to improve efficiency. *)

open Pk

type t

(** The SPP [⊥]. *)
val skip : t

(** The SPP [⊤]. *)
val drop : t

(** [filter b f v] is the SPP [f=v] if [b] is [true] and is the SPP [f≠v] if otherwise. *)
val filter : bool -> field -> value -> t

(** [modf f v] is the SPP [f←v]. *)
val modf : field -> value -> t

(** [mk f m b d] is the SPP [SPP(f, m, b, d)] with field [f], non-default cases [m], default assignments [b] and default identity [d]. *)
val mk : field * ((t Value.M.t) Value.M.t) * (t Value.M.t) * t -> t

(** [to_exp s] is the NetKAT expression corresponding to the SPP [s]. *)
val to_exp : t -> Nk.t

(** [to_exp s] is the string representation of the NetKAT expression corresponding to the SPP [s]. *)
val to_string : t -> string

(** [of_sp s] is the SPP semantically equivalent to [s]; that is, [of_sp s] contains the packet pairs [(pk, pk)]  if and only if [pk ∈ \[\[s\]\]]. *)
val of_sp : Sp.t -> t

(** The comparator of SPPs. *)
val compare : t -> t -> int

(** The equality relation of SPPs. *)
val eq : t -> t -> bool

(** [union_pair s1 s2] is the union of the SPPs [s1] and [s2]. *)
val union_pair : t -> t -> t

(** [union [s1;s2;...;sn]] is the union of the SPPs [s1], [s2], ..., and [sn]. *)
val union : t list -> t

(** [seq_pair s1 s2] is the concatenation cf the SPPs [s1] and [s2] in that order. *)
val seq_pair : t -> t -> t

(** [seq [s1;s2;...;sn]] is the concatenation cf the SPPs [s1], [s2], ..., and [sn] in that order. *)
val seq : t list -> t

(** [intersect_pair s1 s2] is the intersection of the SPPs [s1] and [s2]. *)
val intersect_pair : t -> t -> t

(** [intersect [s1;s2;...;sn]] is the intersection of the SPPs [s1], [s2], ..., and [sn]. *)
val intersect : t list -> t

(** [xor s1 s2] is the symmetric difference [s1 ⊕ s2] between the SPPs [s1] and [s2]. *)
val xor : t -> t -> t

(** [diff s1 s2] is the difference (i.e. [s1 \ s2]) of the SPPs [s1] and [s2]. *)
val diff : t -> t -> t

(** [star s] is the iteration (i.e. [*s]) of the SPP [s]. *)
val star : t -> t

(** [push sp s] is the SP that contains all the packets produced by applying the SPP [s] to the SP [sp]. *)
val push : Sp.t -> t -> Sp.t

(** [pull s sp] is the SP containing all the packets that could have produced [sp] after applying [s]. *)
val pull : t -> Sp.t -> Sp.t

(** [mem s pkpair] indicates whether the packet pair [pkpair] can be produced by [s]. *)
val mem : t -> Pkpair.t -> bool

(** [rep s fset] is a packet pair with fields in [fset] that can be produced by [s]. *)
val rep : t -> Field.S.t -> Pkpair.t

(** [size s] is the numerical size of the SPP [s]. The size [s] is recursively computed by counting the number of branches. *)
val size : t -> int

(** [tikz s] generates the tikz encoding of the SPP [s] used in visualizations. *)
val tikz : t -> string

(** [gv s] generates the gv encoding of the SPP [s] used in visualizations. *)
val gv : t -> string

(** [values_for spp f] returns a set of all the values mentioned (i.e., tested
    or assigned) in [spp] for field [f]. *)
val values_for : t -> field -> Value.S.t

(** [dump ()] clears the pool of hash-consed SPPs. *)
val dump: unit -> unit
