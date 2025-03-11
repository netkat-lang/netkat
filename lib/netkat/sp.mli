(** Symbolic Packet (SP) *)

type sp =
  | Skip 
  | Drop 
  | Union of Field.t * (sp ref Value.M.t) * sp ref * int 

type t = sp ref 

val of_pk : Pk.t -> t

val get_hash : sp -> int 

val compare : t -> t -> int
val eq : t -> t -> bool

val le : t -> t -> bool

val skip : t
val drop : t
val mk : Field.t * (t Value.M.t) * t -> t

val union_pair : t -> t -> t
val union : t list -> t

val seq_pair : t -> t -> t
val seq : t list -> t

val intersect_pair : t -> t -> t
val intersect : t list -> t

val star : t -> t

val diff : t -> t -> t
val neg : t -> t

val xor : t -> t -> t

val rep : t -> Field.S.t -> Pk.t

(*---------- Output ------------------ *)
val to_exp : t -> Nk.t
val to_string : t -> string

val dump: unit -> unit 
