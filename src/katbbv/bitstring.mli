
type test = { var: string; value: bool list }
type act = test
type bexp = test Kat.Ast.bexp
type exp = (act, test) Kat.Ast.exp

(* [of_bexp_katbb bexp] is the Boolean KAT+B! expression representing [bexp]  *)
val of_bexp_katbb : bexp -> Katbb_lib.Ast.bexp

(* [of_exp_katbb exp] is the KAT+B! expression representing [exp] *)
val of_exp_katbb : exp -> Katbb_lib.Ast.exp

