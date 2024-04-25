(** For packet related operations *)

type field = Field.t
type value = Value.t



(* Move these to field/value mli's

(** Convert int to value. *)

*)


(* Operations for concrete packets *)

type t = value Field.M.t

