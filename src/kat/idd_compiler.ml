open Idds
open Hom

type b = Bdd.t
type k = Idd.t

(** [get_ba mgr] produces an instance of [Bdd.t ba] given a BDD manager [mgr] *)
let get_ba mgr = {
    ctrue = Bdd.ctrue;
    cfalse = Bdd.cfalse;
    conj = Bdd.conj mgr;
    disj = Bdd.disj mgr;
    neg = Bdd.neg mgr;
  }

(** [interp_test_mgr mgr interp_fields test] is the BDD resulting from testing
    the variable specificed by [interp_fields test.name] according to 
    [test.value] *)
let interp_test_mgr mgr interp_fields ({ name; value }:Packet_ast.test) = 
  Bdd.test mgr (Var.inp (interp_fields name)) value

let compile_bexp mgr ~interp_fields bexp = 
  let ba = get_ba mgr in
  let interp_test = interp_test_mgr mgr interp_fields in
  interp_bexp ~ba ~interp_test bexp

let compile_exp mgr ~interp_fields exp =
  let bdd_mgr = Idd.get_bdd_manager mgr in
  let ba = get_ba bdd_mgr in
  let kat = {
    ba = ba;
    assrt = Idd.of_bdd;
    union = Idd.union mgr;
    seq = Idd.seq mgr;
    star = Idd.star mgr;
  } in
  let interp_test = interp_test_mgr bdd_mgr interp_fields in
  let interp_act ({ name; value }:Packet_ast.act) = 
    Idd.set mgr (interp_fields name) value in
  interp_exp ~kat ~interp_test ~interp_act exp