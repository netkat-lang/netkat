
open Base

type test = { var: string; value: bool list }
and act = test
and bexp = test Kat.Ast.bexp
and exp = (act, test) Kat.Ast.exp
  [@@deriving sexp, compare, equal, hash]

let ba : Katbb_lib.Ast.bexp Kat.Hom.ba = {
  ctrue = Kat.Ast.True;
  cfalse = Kat.Ast.False;
  conj = (fun a b -> Kat.Ast.Conj (a,b));
  disj = (fun a b -> Kat.Ast.Disj (a,b));
  neg = (fun a -> Kat.Ast.Neg a);
}

let kat : (Katbb_lib.Ast.exp, Katbb_lib.Ast.bexp) Kat.Hom.kat = {
  ba = ba;
  assrt = (fun a -> Kat.Ast.Assert a);
  union = (fun a b -> Kat.Ast.Union (a,b));
  seq = (fun a b -> Kat.Ast.Seq (a,b));
  star = (fun a -> Kat.Ast.Star a);
}

let map_test { var; value } = 
  List.mapi value
    ~f:(fun i bl -> 
      Kat.Ast.Test Katbb_lib.Ast.{ var = var ^ Int.to_string i; value = bl})
  |> Kat.Optimize.big_conj

let map_act { var; value } = 
  List.mapi value 
    ~f:(fun i bl -> 
      Kat.Ast.Action Katbb_lib.Ast.{ var = var ^ Int.to_string i; value = bl})
  |> Kat.Optimize.big_union

let of_bexp_katbb = Kat.Hom.map_bexp ~ba ~map_test

let of_exp_katbb = Kat.Hom.map_exp ~kat ~map_test ~map_act