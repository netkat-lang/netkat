(** Represents a context/environment for names of NKPL values and expressions *)

open Pk

type t

(** empty environment with no bindings*)
val empty : t

(** [bind_exp env name exp] adds [exp] to [env] bound to the name [name].*)
val bind_exp : t -> string -> Nk.t -> t

(** [lookup_exp env name] returns the expression binding for [name] in [env].*)
val lookup_exp : t -> string -> Nk.t

(** [bind_val env name v] adds [v] to [env] bound to the name [name].*)
val bind_val : t -> string -> value -> t

(** [lookup_val env name] returns the value binding for [name] in [env].*)
val lookup_val : t -> string -> value
