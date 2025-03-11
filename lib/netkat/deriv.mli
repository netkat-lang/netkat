(** Compute Brzozowski derivatives of NetKAT terms. *)

(** [d exp] gives an STS representing δ(exp) following section 4.2. of [1]. *)
val d : Nk.t -> Sts.t

(** [e exp] gives an STS representing ε(exp) following section 4.2. of [1]. *)
val e  : Nk.t -> Spp.t


(** [1] Mark Moeller, Jules Jacobs, Olivier Savary Belanger, David Darais, Cole Schlesinger, Steffen Smolka, Nate Foster,
and Alexandra Silva. 2024. KATch: A Fast Symbolic Verifier for NetKAT. In Proceedings of the 41st ACM SIGPLAN
Conference on Programming Language Design and Implementation (Copenhagen, DK) (PLDI 2024). Association for
Computing Machinery, New York, NY, USA. https://doi.org/10.1145/3656454 *)
