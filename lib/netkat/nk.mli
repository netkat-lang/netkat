(** Representation of a Netkat program *)
open Pk

(* Not sure yet whether to expose the constructors or hide (to force smart
constructors). For now it is exposed so that Deriv can match on the variants.*)
type t = 
  | Drop
  | Skip
  | Dup
  | Filter of bool * field * value
  | Mod of field * value
  | Seq of t list
  | Union of t list
  | Star of t
  | Intersect of t list
  | Diff of t * t

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [eq r] decides if the two regexs are *syntactically* equal *)
val eq : t -> t -> bool


(*---------------------- Smart constructors: ---------------------- *)
val skip : t
val drop : t
val dup : t
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
val diff : t -> t -> t

(** Construct a netkat expression which is the symmetric difference (as sets)
    of two netkat expressions *)
val xor : t -> t -> t

(** Construct a netkat expression which is the negation of this expression. Note
    that this fails for expression which are not SPs *)
val neg : t -> t

(** [rand fields values k] constructs a random netkat expression over
    the give [fields] and [values]. The unit interval parameter [k]
    controls the complexity of the expression by determining how many
    AST levels to extend the the "depth" of the expression tree growing by 1
    level.*)
val rand : field list -> value list -> int -> t

(** Pretty print the netkat expression. *)
val to_string : t -> string
