(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Prints of string
  | Tikz of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Value.t
  | Rep of Nkexp.t
  | For of string * int * int * t

(** Pretty prints the netkat expression. *)
val to_string : t -> string

val get_field_vals : Env.t -> t -> Value.S.t Field.M.t

val get_field_vals_from_cmds : Env.t -> t list -> Value.S.t Field.M.t

val expect_val : Env.nk_val -> Value.t
