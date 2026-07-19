(** Represents a context/environment for names of NKPL values and expressions. *)

open Pk

type t
type access = ReadOnly | ReadWrite
type action = Normal | Force
type nk_val = Num of value | Expr of Nk.t | Closure of t * string * Nkexp.t

(** empty environment with no bindings. *)
val empty : t

(** [bind_exp env name exp] adds [exp] to [env] bound to the name [name]. *)
val bind_val : t -> string -> nk_val -> t
val bind_val_access : t -> string -> nk_val -> access -> action -> t

(** [lookup_exp env name] returns the expression binding for [name] in [env]. *)
val lookup_val : t -> string -> nk_val
val lookup_val_opt : t -> string -> nk_val option

val to_string : t -> string
