
open Base

type test = { var: string; value: bool list }
and act = test
and bexp = test Kat.Ast.bexp
and exp = (act, test) Kat.Ast.exp
  [@@deriving sexp, compare, equal, hash]

let ba : Ast.bexp Kat.Hom.ba = {
  ctrue = Kat.Ast.True;
  cfalse = Kat.Ast.False;
  conj = (fun a b -> Kat.Ast.Conj (a,b));
  disj = (fun a b -> Kat.Ast.Disj (a,b));
  neg = (fun a -> Kat.Ast.Neg a);
}

let kat : (Ast.exp, Ast.bexp) Kat.Hom.kat = {
  ba = ba;
  assrt = (fun a -> Kat.Ast.Assert a);
  union = (fun a b -> Kat.Ast.Union (a,b));
  seq = (fun a b -> Kat.Ast.Seq (a,b));
  star = (fun a -> Kat.Ast.Star a);
}

let map_test { var; value } = 
  let make_test (v:string) (i:int) (bl:bool) : Ast.bexp = 
    Kat.Ast.Test { var = v ^ Int.to_string i; value = bl } in
  let (_, e) = List.fold_left value 
    ~init:(0, Kat.Ast.True)
    ~f:(fun (i, acc) bl -> (i+1, Kat.Ast.Conj (acc, (make_test var i bl))))
    in
  e

let map_act { var; value } = 
  let make_act (v:string) (i:int) (bl:bool) : Ast.exp =
    Kat.Ast.Action { var = v ^ Int.to_string i; value = bl } in
  let (_, e) = List.fold_left value
    ~init:(0, Kat.Ast.Assert (Kat.Ast.True))
    ~f:(fun (i, acc) bl -> (i+1, Kat.Ast.Union (acc, make_act var i bl)))
    in
  e

let of_bexp_katbb = Kat.Hom.map_bexp ~ba ~map_test

let of_exp_katbb = Kat.Hom.map_exp ~kat ~map_test ~map_act
