open Idds

type b = Bdd.t
type k = Idd.t

val compile_bexp : Bdd.manager -> interp_fields:(string -> int) 
  -> Packet_ast.bexp -> b

val compile_exp : Idd.manager -> interp_fields:(string -> int) 
  -> Packet_ast.exp -> k