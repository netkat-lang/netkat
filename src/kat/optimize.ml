open Base
open Ast

let ctrue = True
let cfalse = False

let skip = Assert ctrue
let abort = Assert cfalse


let conj b1 b2 =
  match b1, b2 with
  | (False as b), _ | _, (False as b)
  | True, b | b, True ->
    b
  | _, _ ->
    if phys_equal b1 b2 then b1 else
    Conj (b1, b2)

let disj b1 b2 =
  match b1, b2 with
  | (True as b), _ | _, (True as b)
  | False, b | b, False ->
    b
  | _, _ ->
    if phys_equal b1 b2 then b1 else
    Disj (b1, b2)

let neg b =
  match b with
  | True -> cfalse
  | False -> ctrue
  | Neg b -> b
  | _ -> Neg b


let assrt b = Assert b

let union e1 e2 =
  match e1, e2 with
  | Assert False, b | b, Assert False ->
    b
  | _, _ ->
    if phys_equal e1 e2 then e1 else
    Union (e1, e2)

let seq e1 e2 =
  match e1, e2 with
  | (Assert False as e), _ | _, (Assert False as e)
  | Assert True, e | e, Assert True ->
    e
  | _, _ ->
    Seq (e1, e2)

let star e =
  match e with
  | Assert _ -> skip
  | Star _ -> e
  | _ -> Star e


let optimize_bexp ?(negate=false) b =
  let rec opt neg = function
    | True -> if neg then cfalse else ctrue
    | False -> if neg then ctrue else cfalse
    | Test _ as b -> if neg then (Neg b) else b
    | Conj (b1, b2) -> conj (opt neg b1) (opt neg b2)
    | Disj (b1, b2) -> disj (opt neg b1) (opt neg b2)
    | Neg b -> opt (not neg) b
  in
  opt negate b

let rec optimize_exp e =
  match e with
  | Assert b -> assrt (optimize_bexp b)
  | Action _ -> e
  | Union (e1, e2) -> union (optimize_exp e1) (optimize_exp e2)
  | Seq (e1, e2) -> seq (optimize_exp e1) (optimize_exp e2)
  | Star e -> star (optimize_exp e)
