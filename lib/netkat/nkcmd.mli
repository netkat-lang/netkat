(** Representation of a Netkat command *)

type t =
  | Import of string
  | Check of bool * Nk.t * Nk.t
  | Print of Nk.t
  | Let of string * Nk.t
  (* | For of string * value * value * t *) (* TODO *)

(** Interpret / execute the nkpl command *)
val interp : t -> unit

(** Pretty print the netkat expression. *)
val to_string : t -> string
