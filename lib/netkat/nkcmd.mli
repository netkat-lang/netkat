(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Let of string * Nkexp.t
  (* | For of string * value * value * t *) (* TODO *)

(** Interpret / execute the nkpl command *)
val interp : t -> unit

(** Pretty print the netkat expression. *)
val to_string : t -> string
