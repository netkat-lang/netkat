
(* --- Brzozowski derivatives --- *)
(** [d e] gives an STS representing δ(e) following section 4.2. *)
val d : Nkexp.t -> Sts.t
val e  : Nkexp.t -> Spp.t
