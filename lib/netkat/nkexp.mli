(** Representation of a Netkat program *)
open Pk

(* Not sure yet whether to expose the constructors or hide (to force smart
constructors). For now it is exposed so that Deriv can match on the variants.*)
type t = 
  | Exp of Nk.t
  | Neg of t
  | Fwd of t
  | Bwd of t
  (* | Range of *) (* TODO *)
  | Exists of field * t
  | Forall of field * t

(** Provides a comparison using the standard interface to [compare] *)
val compare : t -> t -> int

(** [eq r] decides if the two regexs are *syntactically* equal *)
val eq : t -> t -> bool


(*---------------------- Smart constructors: ---------------------- *)
(*
val fwd : t -> t
val bwd : t -> t
val exists : field -> t -> t
val forall : field -> t -> t
*)

