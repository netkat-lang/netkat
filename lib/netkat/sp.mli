(** Representation of a Symbolic Packet (SP). Hash-consing is used in this module to improve efficiency. *)

type sp =
  | Skip 
  | Drop 
  | Union of Field.t * (sp ref Value.M.t) * sp ref * int 

type t = sp ref 
(** The actual exposed type of SPs in the hash-consing scheme. *)

(** [of_pk p] is the symbolic packet representing the single concrete packet [p].*)
val of_pk : Pk.t -> t

(** [get_hash sp] is the hash of the SP [sp] in the hash-consing scheme. *)
val get_hash : sp -> int 

(** The comparator for SPs. *)
val compare : t -> t -> int

(** The equality relation for SPs. *)
val eq : t -> t -> bool

(** The less-than-or-equal-to relation for SPs. *)
val le : t -> t -> bool

(** The SPP [⊤]. *)
val skip : t

(** The SP [⊥]. *)
val drop : t

(** [mk f m d] constructs the SP corresponding to the field [f], non-default cases [m] and the default-case [d]*)
val mk : Field.t * (t Value.M.t) * t -> t

(** [union_pair sp1 sp2] is the union of the SPs [sp1] and [sp2].*)
val union_pair : t -> t -> t

(** [union [sp1;sp2;...;spn]] is the union of the SPs [sp1], [sp2], ..., and [spn]. *)
val union : t list -> t

(** [seq_pair sp1 sp2] is the concatenation cf the SPs [sp1] and [sp2] in that order.*)
val seq_pair : t -> t -> t

(** [seq [sp1;sp2;...;spn]] is the concatenation cf the SPs [sp1], [sp2], ..., and [spn] in that order.*)
val seq : t list -> t

(** [intersect_pair sp1 sp2] is the intersection of the SPs [sp1] and [sp2].*)
val intersect_pair : t -> t -> t

(** [intersect [sp1;sp2;...;spn]] is the intersection of the SPs [sp1], [sp2], ..., and [spn]. *)
val intersect : t list -> t

(** [star sp] is the iteration (i.e. [*sp]) of the SP [sp]. *)
val star : t -> t

(** [diff sp1 sp2] is the difference (i.e. [sp1 \ sp2]) of [sp1] and [sp2]*)
val diff : t -> t -> t

(** [neg sp1] is the negation of [sp1]. *)
val neg : t -> t

(** [xor sp1 sp2] is the symmetric difference [sp1 ⊕ sp2] between [sp1] and [sp2]*)
val xor : t -> t -> t

(** [rep sp fields] is a packet in the SP [sp] with fields [fields]. *)
val rep : t -> Field.S.t -> Pk.t

(*---------- Output ------------------ *)
(** [to_exp sp] is the NetKAT expression corresponding to the SP [sp]. *)
val to_exp : t -> Nk.t

(** [to_string sp] is a string representation of the NetKAT expression corresponding to the SP [sp]. *)
val to_string : t -> string

(** [dump ()] clears the pool of hash-consed SPs. *)
val dump: unit -> unit 
