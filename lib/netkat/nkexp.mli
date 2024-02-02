(** Representation of a Netkat program *)
open Pk

type t

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [eq r] decides if the two regexs are *syntactically* equal *)
val eq : t -> t -> bool


(*---------------------- Smart constructors: ---------------------- *)
val neg : t -> t
val fwd : t -> t
val bwd : t -> t
val exists : field -> t -> t
val forall : field -> t -> t

val skip : t
val drop : t
val dup : t
val var : string -> t
val filter : bool -> field -> value -> t
val modif : field -> value -> t

(** Construct a netkat expression which is the concatenation of a list of
    netkat expressions. *)
val seq : t list -> t

(** Construct a netkat expression which is the concatentation of the two
    given netkat expressions. *)
val seq_pair : t -> t -> t

(** Construct a netkat expression which is the union of a list of
    netkat expressions. *)
val union : t list -> t

(** Construct a netkat expression which is the union of the two
    given netkat expressions. *)
val union_pair : t -> t -> t

(** Construct a netkat expression which is the intersection of a list of
    netkat expressions. *)
val intersect : t list -> t

(** Construct a netkat expression which is the intersection of the two
    given netkat expressions. *)
val intersect_pair : t -> t -> t

(** Construct a netkat expression which is the Kleene star of the given
    netkat expression. *)
val star : t -> t

(** Construct a netkat expression which is the difference (as sets) of first
    netkat expression and the second one. *)
val difference : t -> t -> t

(** Construct a netkat expression which is the symmetric difference (as sets)
    of two netkat expressions *)
val xor : t -> t -> t

(** Pretty print the netkat expression. *)
val to_string : t -> string
(** Evaluate a Nkexp to a fully formed Nk term. *)
val eval : t -> Nk.t
