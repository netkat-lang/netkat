(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Tikz of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Pk.value
  | Rep of Nkexp.t
  (* | For of string * value * value * t *) (* TODO *)

(** Pretty print the netkat expression. *)
val to_string : t -> string
