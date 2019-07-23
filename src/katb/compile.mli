open Idds
open Ast

(** [compile_pol mgr label expr] is the IDD representing [expr] where [label]
    maps program variables to their respective DD variable indices.
    This mapping is required to be injective (or else the result is undefined).
*)
val compile_pol : Idd.manager -> label:(string -> int) -> expr -> Idd.t

(** [compile_pred mgr label expr] is the BDD representing [expr] where [label]
    maps program variables to their respective DD variable indices. 
    This mapping is required to be injective (or else the result is undefined).
*)
val compile_pred : Bdd.manager -> label:(string -> int) -> bexpr -> Bdd.t