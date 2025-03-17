(** AST for NKPL program *)

open Pk

type t

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [eq r] decides if the two regexs are *syntactically* equal *)
val eq : t -> t -> bool

(*---------------------- Smart constructors: ---------------------- *)
(** Node in AST for NetKAT negation. *)
val neg : t -> t

(** Node in AST for invoking the forward algorithm*)
val fwd : t -> t

(** Node in AST for invoking the backward algorithm*)
val bwd : t -> t

(** Node in AST for existential quantifiers *)
val exists : field -> t -> t

(** Node in AST for universal quantifiers *)
val forall : field -> t -> t

(** Node in AST for NetKAT skip. *)
val skip : t

(** Node in AST for NetKAT drop. *)
val drop : t

(** Node in AST for NetKAT dup. *)
val dup : t

(** Node in AST for a variable. *)
val var : string -> t

(** Node in AST for NetKAT filters *)
val filter : bool -> field -> value -> t

(** Node in AST for NetKAT modifications. *)
val modif : field -> value -> t

(** Node in AST for NetKAT modifications with variables in place for constants.*)
val vfilter : bool -> field -> string -> t

(** Node in AST for NetKAT modifications with variables in place for constants. *)
val vmodif : field -> string -> t

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

(** Construct a netkat expression which is the symmetric difference (as sets)
    of two netkat expressions *)
val xor : t -> t -> t

(** Construct a netkat expression which is the difference (as sets)
    of two netkat expressions *)
val diff : t -> t -> t

(** Pretty print the netkat expression. *)
val to_string : t -> string

(** Evaluate a Nkexp to a fully formed Nk term. *)
val eval : Env.t -> t -> Nk.t
