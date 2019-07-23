(** Generic AST for Kleene Algebra with Tests (KAT). *)


(** {1 Expressions} *)

(** Boolean expressions over primitive tests ['test]. *)
type 'test bexp =
  | True
  | False
  | Test of 'test
  | Conj of ('test bexp) * ('test bexp)
  | Disj of ('test bexp) * ('test bexp)
  | Neg of 'test bexp

(** KAT expressions over actions ['act] and primitive tests ['test]. *)
and ('act, 'test) exp =
  | Assert of 'test bexp
  | Action of 'act
  | Union of ('act, 'test) exp * ('act, 'test) exp
  | Seq of ('act, 'test) exp * ('act, 'test) exp
  | Star of ('act, 'test) exp
  [@@deriving sexp, compare, equal, hash]