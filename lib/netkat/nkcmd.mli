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

val get_fields : t -> Field.S.t
val get_values : t -> Value.S.t

val get_fields_from_cmds : t list -> Field.S.t
val get_values_from_cmds : t list -> Value.S.t
