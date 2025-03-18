(** Symbolic Transition System (STS) *)

type t

(** [trans e t] is the transition in [t] for the Netkat expression [e]. *)
val trans : Nk.t -> t -> Spp.t

(** [to_list s] converts the STS [s = p1.dup.q1 + ... + pn.dup.qn] to the list of tuples [[(q1, p1);...;(qn,pn)]]. *)
val to_list : t -> (Nk.t * Spp.t) list

(** [drop] is the STS [⊤.dup.⊥]. *)
val drop : t

(** [dup] is the STS [⊤.dup.⊤]. *)
val dup : t

(** [union [t1;t2;...;tn]] is the union of the SPPs [t1], [t2], ..., and [tn]. *)
val union : t list -> t

(** [union_pair t1 t2] is the union of the STSs [t1] and [t2]. *)
val union_pair : t -> t -> t

(** [intersect_pair t1 t2] is the intersection of the STSs [t1] and [t2]. *)
val intersect_pair : t -> t -> t

(** [intersect [t1;t2;...;tn]] is the intersection of the STSs [t1], [t2], ..., and [tn]. *)
val intersect : t list -> t

(** [seq_spp s t] concatenates the SPP [s] before each term in the STS [t]. *)
val seq_spp : Spp.t -> t -> t

(** [sep_exp e t] concatenates the NetKAT expression [e] after each term in the STS [t]. *)
val seq_exp : t -> Nk.t -> t

(** [diff t1 t2] is the difference (i.e. [t1 \ t2]) of the STSs [t1] and [t2]. *)
val diff : t -> t -> t

(** [xor t1 t2] is the symmetric difference [t1 ⊕ t2] between the STSs [t1] and [t2]. *)
val xor : t -> t -> t

(** [to_exp t] is the NetKAT expression corresponding to the STS [t]. *)
val to_exp : t -> Nk.t

(** [to_string t] is the string representation of the NetKAT expression corresponding to the STS [t]. *)
val to_string : t -> string

