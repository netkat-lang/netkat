open Ego.Generic

open Sexplib0

let compare_value = Value.compare
let compare_field = Field.compare
let pp_value fmt v = Format.pp_print_string fmt (Value.to_string v)
let pp_field fmt f = Format.pp_print_string fmt (Field.get_or_fail_fid f)

module L = struct

  type 'a shape =
  | Drop
  | Skip
  | Dup
  | Star of 'a
  | Neg of 'a
  | Fwd of 'a
  | Bwd of 'a
  | Union of 'a list
  | Seq of 'a list
  | Intersect of 'a list
  | PosFilter of 'a * 'a
  | NegFilter of 'a * 'a
  | Mod of 'a * 'a
  | PosVFilter of 'a * 'a
  | NegVFilter of 'a * 'a
  | VMod of 'a * 'a
  | Xor of 'a * 'a
  | Diff of 'a * 'a
  | Exists of 'a * 'a
  | Forall of 'a * 'a
  | Var of string
  | Const of int
  [@@deriving ord, show]

  type op =
    | DropOp
    | DupOp
    | SkipOp
    | StarOp
    | NegOp
    | FwdOp
    | BwdOp
    | UnionOp
    | SeqOp
    | IntersectOp
    | FilterOp of bool
    | ModOp
    | VFilterOp of bool
    | VModOp
    | XorOp
    | DiffOp
    | ExistsOp
    | ForallOp
    | VarOp of string
    | ConstOp of int
    [@@deriving eq]

  type t = Mk of t shape  [@@unboxed]

  let rec of_sexp = function [@warning "-8"]
    | Sexplib0.Sexp.Atom s -> (
      match s with
      | "drop" -> Mk Drop
      | "skip" -> Mk Skip
      | "dup" -> Mk Dup
      | _ -> (
      match int_of_string_opt s with
      | Some n -> Mk (Const n)
      | None -> Mk (Var s)
    ))

    | List [Atom "star"; l] -> Mk (Star (of_sexp l))
    | List [Atom "not"; l] -> Mk (Neg (of_sexp l))
    | List [Atom "forward"; l] -> Mk (Fwd (of_sexp l))
    | List [Atom "backward"; l] -> Mk (Bwd (of_sexp l))
    | List ((Atom "union")::l) -> Mk (Union (List.map of_sexp l))
    | List ((Atom "seq")::l) -> Mk (Seq (List.map of_sexp l))
    | List ((Atom "intersection")::l) -> Mk (Intersect (List.map of_sexp l))
    | List [Atom "eq"; l; r] -> Mk (PosFilter (of_sexp l, of_sexp r))
    | List [Atom "neq"; l; r] -> Mk (NegFilter (of_sexp l, of_sexp r))
    | List [Atom "set"; l; r] -> Mk (Mod (of_sexp l, of_sexp r))
    | List [Atom "veq"; l; r] -> Mk (PosVFilter (of_sexp l, of_sexp r))
    | List [Atom "vneq"; l; r] -> Mk (NegVFilter (of_sexp l, of_sexp r))
    | List [Atom "vset"; l; r] -> Mk (VMod (of_sexp l, of_sexp r))
    | List [Atom "diff"; l; r] -> Mk (Diff (of_sexp l, of_sexp r))
    | List [Atom "xor"; l; r] -> Mk (Xor (of_sexp l, of_sexp r))
    | List [Atom "exists"; l; r] -> Mk (Exists (of_sexp l, of_sexp r))
    | List [Atom "forall"; l; r] -> Mk (Forall (of_sexp l, of_sexp r))

  let rec to_sexp = function
    | Mk (Drop) -> Sexplib0.Sexp.Atom "drop"
    | Mk (Skip) -> Sexplib0.Sexp.Atom "skip"
    | Mk (Dup) -> Sexplib0.Sexp.Atom "dup"
    | Mk (Star (l)) -> List [Atom "star"; to_sexp l]
    | Mk (Neg (l)) -> List [Atom "not"; to_sexp l]
    | Mk (Fwd (l)) -> List [Atom "forward"; to_sexp l]
    | Mk (Bwd (l)) -> List [Atom "backward"; to_sexp l]
    | Mk (Union (l)) -> List ((Atom "union")::(List.map to_sexp l))
    | Mk (Seq (l)) -> List ((Atom "seq")::(List.map to_sexp l))
    | Mk (Intersect (l)) -> List ((Atom "intersection")::(List.map to_sexp l))
    | Mk (PosFilter (l, r)) -> List [Atom "eq"; to_sexp l; to_sexp r]
    | Mk (NegFilter (l, r)) -> List [Atom "neq"; to_sexp l; to_sexp r]
    | Mk (Mod (l, r)) -> List [Atom "set"; to_sexp l; to_sexp r]
    | Mk (PosVFilter (l, r)) -> List [Atom "veq"; to_sexp l; to_sexp r]
    | Mk (NegVFilter (l, r)) -> List [Atom "vneq"; to_sexp l; to_sexp r]
    | Mk (VMod (l, r)) -> List [Atom "vset"; to_sexp l; to_sexp r]
    | Mk (Xor (l, r)) -> Sexplib0.Sexp.List [Atom "xor"; to_sexp l; to_sexp r]
    | Mk (Diff (l, r)) -> List [Atom "diff"; to_sexp l; to_sexp r]
    | Mk (Exists (l, r)) -> List [Atom "exists"; to_sexp l; to_sexp r]      
    | Mk (Forall (l, r)) -> List [Atom "forall"; to_sexp l; to_sexp r]      
    | Mk (Var s) -> Atom s
    | Mk (Const n) -> Atom (Int.to_string n)

  let op = function
    | Drop -> DropOp
    | Skip -> SkipOp
    | Dup -> DupOp
    | Star _ -> StarOp
    | Neg _ -> NegOp
    | Fwd _ -> FwdOp
    | Bwd _ -> BwdOp
    | Union _ -> UnionOp
    | Seq _ -> SeqOp
    | Intersect _ -> IntersectOp
    | PosFilter _ -> FilterOp true
    | NegFilter _ -> FilterOp false
    | Mod _ -> ModOp
    | PosVFilter _ -> VFilterOp true
    | NegVFilter _ -> VFilterOp false
    | VMod _ -> VModOp
    | Xor _ -> XorOp
    | Diff _ -> DiffOp
    | Exists _ -> ExistsOp
    | Forall _ -> ForallOp
    | Var s -> VarOp s
    | Const i -> ConstOp i

  let op_of_string = function
    | "drop" -> DropOp
    | "skip" -> SkipOp
    | "dup" -> DupOp
    | "star" -> StarOp
    | "not" -> NegOp
    | "forward" -> FwdOp
    | "backward" -> BwdOp
    | "union" -> UnionOp
    | "seq" -> SeqOp
    | "intersection" -> IntersectOp
    | "eq" -> FilterOp true
    | "neq" -> FilterOp false
    | "set" -> ModOp
    | "veq" -> VFilterOp true
    | "vneq" -> VFilterOp false
    | "vset" -> VModOp
    | "xor" -> XorOp
    | "diff" -> DiffOp
    | "exists" -> ExistsOp
    | "forall" -> ForallOp
    | s -> match int_of_string_opt s with
      | None -> VarOp s
      | Some n -> ConstOp n

  let children = function
    | Drop -> []
    | Skip -> []
    | Dup -> []
    | Star (l) -> [l]
    | Neg (l) -> [l]
    | Fwd (l) -> [l]
    | Bwd (l) -> [l]
    | Union (l) -> l
    | Seq (l) -> l
    | Intersect (l) -> l
    | PosFilter(l,r) -> [l;r]
    | NegFilter(l,r) -> [l;r]
    | Mod(l,r) -> [l;r]
    | PosVFilter(l,r) -> [l;r]
    | NegVFilter(l,r) -> [l;r]
    | VMod(l,r) -> [l;r]
    | Xor (l,r)
    | Diff (l,r)
    | Exists (l,r) -> [l;r]
    | Forall (l,r) -> [l;r]
    | Var _
    | Const _ -> []

  let map_children term f = match term with
    | Drop -> Drop
    | Skip -> Skip
    | Dup -> Dup
    | Star (l) -> Star (f l)
    | Neg (l) -> Neg (f l)
    | Fwd (l) -> Fwd (f l)
    | Bwd (l) -> Bwd (f l)
    | Union (l) -> Union (List.map f l)
    | Seq (l) -> Seq (List.map f l)
    | Intersect (l) -> Intersect (List.map f l)
    | PosFilter (l,r) -> PosFilter (f l,f r)
    | NegFilter (l,r) -> NegFilter (f l,f r)
    | Mod (l,r) -> Mod (f l,f r)
    | PosVFilter (l,r) -> PosVFilter (f l,f r)
    | NegVFilter (l,r) -> NegVFilter (f l,f r)
    | VMod (l,r) -> VMod (f l,f r)
    | Xor (l,r) -> Xor (f l, f r)
    | Diff (l,r) -> Diff (f l, f r)
    | Exists (l,r) -> Exists (f l, f r)
    | Forall (l,r) -> Forall (f l, f r)
    | Var s -> Var s
    | Const i -> Const i

  let make op ls =
    match[@warning "-8"] op,ls with
    | DropOp, [] -> Drop
    | SkipOp, [] -> Skip
    | DupOp, [] -> Dup
    | StarOp, [l] -> Star (l)
    | NegOp, [l] -> Neg (l)
    | FwdOp, [l] -> Fwd (l)
    | BwdOp, [l] -> Bwd (l)
    | UnionOp, l -> Union (l)
    | SeqOp, l -> Seq (l)
    | IntersectOp, l -> Intersect (l)
    | FilterOp true, [l;r] -> PosFilter (l,r)
    | FilterOp false, [l;r] -> NegFilter (l,r)
    | ModOp, [l;r] -> Mod (l,r)
    | VFilterOp true, [l;r] -> PosVFilter (l,r)
    | VFilterOp false, [l;r] -> NegVFilter (l,r)
    | VModOp, [l;r] -> VMod (l,r)
    | XorOp, [l;r] -> Xor (l,r)
    | DiffOp, [l;r] -> Diff (l,r)
    | ExistsOp, [l;r] -> Exists (l,r)
    | ForallOp, [l;r] -> Forall (l,r)
    | VarOp s, [] -> Var s
    | ConstOp i, [] -> Const i

end

(* stub cost metric *)
module C = struct
  type t = float [@@deriving ord]
  let cost f : Ego.Id.t L.shape -> t = function
    | Drop
    | Skip
    | Dup -> 0.0
    | Var _ -> 0.0
    | Const k -> 0.0 (* float_of_int k *)
    | Star(l) -> f l +. 1.0
    | Neg(l)
    | Fwd(l)
    | Bwd(l) -> f l +. 1.0
    | Xor(l,r)
    | Diff(l,r)
    | Exists(l,r)
    | Forall(l,r)
    | PosFilter(l,r)
    | NegFilter(l,r)
    | Mod(l,r)
    | PosVFilter(l,r)
    | NegVFilter(l,r)
    | VMod(l,r) -> f l +. f r +. 1.0
    | Union(l)
    | Seq(l)
    | Intersect(l) -> List.fold_left (fun a x -> a +. f x) 1.0 l
  (* let cost f : Ego.Id.t L.shape -> t = (fun x -> -. (cost2 f x)) *)
end

(* stub analysis *)
module A = struct type t = unit type data = int option [@@deriving eq, show] let default = None end
module MA (S : GRAPH_API
           with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
            and type 'a shape := 'a L.shape
            and type analysis := A.t
            and type data := A.data
            and type node := L.t)  = struct
  type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph

  let eval : A.data L.shape -> A.data =
    function
    | _ -> None

  let make : ro t -> Ego.Id.t L.shape -> A.data =
    fun graph term ->
    eval (L.map_children term (S.get_data graph))

  let merge : A.t -> A.data -> A.data -> A.data * (bool * bool) =
    fun () l r ->  match l,r with
      | _ -> None, (false, false)

  let modify : 'a t -> Ego.Id.t -> unit =
    fun graph cls ->
    match S.get_data (S.freeze graph) cls with
    | _ -> ()

end

module EGraph = Make (L) (A) (MA)
module Extractor = MakeExtractor (L) (C)

let make_rule l r =
  let from = Query.of_sexp L.op_of_string l in
  let into = Query.of_sexp L.op_of_string r in
  EGraph.Rule.make_constant ~from ~into

let make_rules ?(bidir = true) l r =
  (make_rule l r)::(if bidir then [make_rule r l] else [])

let make_cond_rule l r cond =
  let from = Query.of_sexp L.op_of_string l in
  let into = Query.of_sexp L.op_of_string r in
  EGraph.Rule.make_conditional ~from ~into ~cond

let make_cond_rules ?(bidir = true) l r cond =
  (make_cond_rule l r cond)::(if bidir then [make_cond_rule r l cond] else [])

let is_distinct v w =
  fun graph _root_id env ->
    let v = StringMap.find v env in
    let w = StringMap.find w env in
    (not @@ EGraph.class_equal (EGraph.freeze graph) v w)

let is_predicate v =
  fun graph _root_id env ->
  let v = StringMap.find v env in
  EGraph.iter_children (EGraph.freeze graph) v |>
  Iter.exists (function
    | L.Drop
    | Skip
    | PosFilter _
    | NegFilter _
    | PosVFilter _
    | NegVFilter _ -> true
    (* TODO XXX - predicates need to somehow be separate from policies? *)
    | _ -> false
  )

let test () =
    (*

    f:=1;(f=1;f:=2 + f=2;f:=3)*;f=3
    f:=1;(skip + (f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
    (f:=1;skip + f:=1;(f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
    (f:=1;skip + (f:=1;f=1;f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3
    (f:=1;skip + (f:=2 + f=2;f:=3);(f=1;f:=2 + f=2;f:=3)* );f=3

    *)

    (* create an egraph *)
    let graph = EGraph.init () in
    (* add expressions *)
    (*let expr1 = [%s (seq (eq a 2) (eq a 2))]*)
    (* let expr1 = [%s (seq (set a 1) (star drop))] *)
    (*let expr1 = [%s (seq (set a 123) (set b a))]*)
    let expr1 = [%s (seq (set a 1) (seq (star (union (seq (eq a 1) (set a 2)) (seq (eq a 2) (set a 3)))) (eq a 3)))] in
    (*let expr1 = Nkexp.to_sexp (Nkexp.Seq([Nkexp.Mod(Field.get_or_assign_fid "a",Value.of_int 2);Nkexp.Mod(Field.get_or_assign_fid "a",Value.of_int 3)]))*)
    (*let expr1 = Nkexp.to_sexp (Nkexp.filter true (Field.get_or_assign_fid "a") (Value.of_int 2))*)
    let expr2 = expr1 (* Sexplib0.Sexp.List [Sexp.Atom "="; Sexp.Atom "f"; Sexp.Atom "1"] *) in
    let e1 = EGraph.add_node graph (L.of_sexp expr1) in
    let e2 = EGraph.add_node graph (L.of_sexp expr2) in
    let rules =
      (* Kleene Algebra Axioms *)
      make_rules ~bidir:true  [%s (union "?a" (union "?b" "?c"))] [%s (union (union "?a" "?b") "?c")] @
      make_rules ~bidir:true  [%s (union "?a" "?b")] [%s (union "?b" "?a")] @
      make_rules ~bidir:true  [%s (union "?a" drop)] [%s "?a"] @
      make_rules ~bidir:true  [%s (union "?a" "?a")] [%s "?a"] @
      make_rules ~bidir:true  [%s (seq "?a" (seq "?b" "?c"))] [%s (seq (seq "?a" "?b") "?c")] @
      make_rules ~bidir:true  [%s (seq skip "?a")] [%s "?a"] @
      make_rules ~bidir:true  [%s (seq "?a" skip)] [%s "?a"] @
      make_rules ~bidir:true  [%s (seq "?a" (union "?b" "?c"))] [%s (union (seq "?a" "?b") (seq "?a" "?c"))] @
      make_rules ~bidir:true  [%s (seq (union "?a" "?b") "?c")] [%s (union (seq "?a" "?c") (seq "?b" "?c"))] @
      make_rules ~bidir:false [%s (seq drop "?a")] [%s drop] @
      make_rules ~bidir:false [%s (seq "?a" drop)] [%s drop] @
      make_rules ~bidir:true  [%s (union skip (seq "?a" (star "?a")))] [%s (star "?a")] @
      make_rules ~bidir:true  [%s (union skip (seq (star "?a") "?a"))] [%s (star "?a")] @
      (* Additional Boolean Algebra Axioms *)
      (*make_rules ~bidir:false [%s (union "?a" skip)] [%s skip] @
      make_rules ~bidir:false [%s (union "?a" (not "?a"))] [%s skip] @
      make_rules ~bidir:false [%s (seq "?a" "?b")] [%s (seq "?b" "?a")] @
      make_rules ~bidir:false [%s (seq "?a" (not "?a"))] [%s drop] @*)
      make_cond_rules ~bidir:true [%s (seq "?a" "?a")] [%s "?a"] (is_predicate "a") @
      (* Packet Algebra Axioms *)
      make_rules ~bidir:true  [%s (seq dup (set "?a" "?b"))] [%s (seq (set "?a" "?b") dup)] @
      make_rules ~bidir:true  [%s (seq (set "?a" "?b") (eq "?a" "?b"))] [%s (set "?a" "?b")] @
      make_rules ~bidir:true  [%s (seq (eq "?a" "?b") (set "?a" "?b"))] [%s (eq "?a" "?b")] @
      make_rules ~bidir:false [%s (seq (set "?a" "?b") (set "?a" "?c"))] [%s (set "?a" "?c")] @
      make_cond_rules ~bidir:false [%s (seq (eq "?a" "?b") (eq "?a" "?c"))] [%s drop] (is_distinct "b" "c") @
      (* Tentative *)
      (* TODO XXX - not quite right *)
      make_rules ~bidir:false [%s (seq (set "?a" "?b") (set "?c" "?a"))] [%s (set "?c" "?b")] @
      [] in
    let _ = EGraph.run_until_saturation ~fuel:(`Bounded 15) graph rules in
    let r = Extractor.extract graph e1 in
    let result = L.to_sexp r in
    let _ = Printf.printf "%s\n" (Sexp.to_string result) in
    let _ = Printf.printf "input: %s\n" (Nkexp.to_string (Nkexp.of_sexp expr1)) in
    let _ = Printf.printf "output: %s\n" (Nkexp.to_string (Nkexp.of_sexp result)) in
    (*let _ = Printf.printf "%s\n" (Nkexp.to_string (Nkexp.of_sexp [%s (seq (set a 1) (set a 2))]))*)
    (* Convert to graphviz *)
    let g : Odot.graph = EGraph.to_dot graph in
    let _ = let c = open_out "test.dot" in Printf.fprintf c "%s" (Odot.string_of_graph g); close_out c in
    ()
    (* dot -Tpdf test.dot -o test.pdf *)
