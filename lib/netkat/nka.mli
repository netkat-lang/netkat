(** Netkat Automaton *)

type t

val autom : Nkexp.t -> t
val bisim : t -> t -> bool
