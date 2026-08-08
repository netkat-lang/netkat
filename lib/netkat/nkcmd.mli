(** Representation of a Netkat command *)

(** A value in a diversify field's [DExplicit] domain, e.g. the
    [Medical_Device] in "@dev=[Medical_Device,Provider_Host]" -- either a
    literal ([Dnum]) or a named constant ([Dvar], e.g. a prior
    "Medical_Device = 5" binding) not resolved until the simulate command
    actually runs, against whatever [Env.t] is current then. *)
type dvalue =
  | Dnum of Value.t
  | Dvar of string

(** How a simulate command's "{...}" list asks for one field to be
    enumerated -- the not-yet-resolved, syntax-level counterpart of
    [Nka.diversify_mode] (resolving [DExplicit]'s [dvalue list] against an
    [Env.t] is what turns this into an actual [Nka.diversify_mode]). *)
type dmode =
  | DBestEffort
  | DExhaustive
  | DExplicit of dvalue list

type t =
  | Import of string
  | Check of string option * bool * Nkexp.t * Nkexp.t
  | Print of Nkexp.t
  | Prints of string
  | Tikz of Nkexp.t
  | Let of string * Nkexp.t
  | VLet of string * Value.t
  | Rep of Nkexp.t
  | Simulate of string option * int option * (Field.t * dmode) list * Nkexp.t option * Nkexp.t
  | For of string * int * int * t

(** Pretty prints the netkat expression. *)
val to_string : t -> string

val get_field_vals : Env.t -> t -> Value.S.t Field.M.t

val get_field_vals_from_cmds : Env.t -> t list -> Value.S.t Field.M.t

val expect_val : Env.nk_val -> Value.t
