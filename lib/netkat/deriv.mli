
(* --- Brzozowski derivatives --- *)
(** [d e] gives an STS representing δ(e) following section 4.2. *)
val d : Nk.t -> Sts.t
val e  : Nk.t -> Spp.t
