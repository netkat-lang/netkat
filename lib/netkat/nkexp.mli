(** Representation of a Netkat program *)

type field
type value

type t =
  | Drop
  | Skip
  | Dup
  | Filter of field * value
  | Mod of field * value
  | Seq of t list
  | Union of t list
  | Star of t
  | Intersect of t list
  | Neg of t
  | Fwd of t
  | Bwd of t
  (* | Range of *) (* TODO *)
  | Exists of field * t
  | Forall of field * t

(** Lookup (or, generate a new) int label for a field name *)
val get_or_assign_fid : string -> field

(** Convert int to value. *)
val value_of_int : int -> value

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [equiv r] decides if the two regexs are *syntactically* equivalent *)
val equiv : t -> t -> bool

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

(** Construct a netkat expression which is the complement of the given netkat
    expression. *)
val neg : t -> t

(** Construct a netkat expression which is the difference (as sets) of first
    netkat expression and the second one. *)
val difference : t -> t -> t

(** Construct a netkat expression which is the symmetric difference (as sets)
    of two netkat expressions *)
val xor : t -> t -> t

(** Pretty print the netkat expression. *)
val to_string : t -> string
