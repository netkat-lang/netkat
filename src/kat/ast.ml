open Base

type 'test bexp =
  | True
  | False
  | Test of 'test
  | Conj of ('test bexp) * ('test bexp)
  | Disj of ('test bexp) * ('test bexp)
  | Neg of 'test bexp
  [@@deriving sexp, compare, equal, hash]

type ('act, 'test) exp =
  | Assert of 'test bexp
  | Action of 'act
  | Union of ('act, 'test) exp * ('act, 'test) exp
  | Seq of ('act, 'test) exp * ('act, 'test) exp
  | Star of ('act, 'test) exp
  [@@deriving sexp, compare, equal, hash]
