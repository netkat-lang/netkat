open Pk
open Ego.Generic

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

    | List [Atom "*"; l] -> Mk (Star (of_sexp l))
    | List [Atom "¬"; l] -> Mk (Neg (of_sexp l))
    | List [Atom "forward"; l] -> Mk (Fwd (of_sexp l))
    | List [Atom "backward"; l] -> Mk (Bwd (of_sexp l))

    | List ((Atom "∪")::l) -> Mk (Union (List.map of_sexp l))
    | List ((Atom "⋅")::l) -> Mk (Seq (List.map of_sexp l))
    | List ((Atom "&")::l) -> Mk (Intersect (List.map of_sexp l))

    | List [Atom "="; l; r] -> Mk (PosFilter (of_sexp l, of_sexp r))
    | List [Atom "≠"; l; r] -> Mk (NegFilter (of_sexp l, of_sexp r))
    | List [Atom "\u{2190}"; l; r] -> Mk (Mod (of_sexp l, of_sexp r))

    | List [Atom "-"; l; r] -> Mk (Diff (of_sexp l, of_sexp r))
    | List [Atom "⊕"; l; r] -> Mk (Xor (of_sexp l, of_sexp r))
    | List [Atom "exists"; l; r] -> Mk (Exists (of_sexp l, of_sexp r))
    | List [Atom "forall"; l; r] -> Mk (Forall (of_sexp l, of_sexp r))

  let rec to_sexp = function
    | Mk (Drop) -> Sexplib0.Sexp.Atom "drop"
    | Mk (Skip) -> Sexplib0.Sexp.Atom "skip"
    | Mk (Dup) -> Sexplib0.Sexp.Atom "dup"

    | Mk (Star (l)) -> List [Atom "*"; to_sexp l]
    | Mk (Neg (l)) -> List [Atom "¬"; to_sexp l]
    | Mk (Fwd (l)) -> List [Atom "forward"; to_sexp l]
    | Mk (Bwd (l)) -> List [Atom "backward"; to_sexp l]

    | Mk (Union (l)) -> List ((Atom "∪")::(List.map to_sexp l))
    | Mk (Seq (l)) -> List ((Atom "&")::(List.map to_sexp l))
    | Mk (Intersect (l)) -> List ((Atom "⋅")::(List.map to_sexp l))

    | Mk (PosFilter (l, r)) -> List [Atom "="; to_sexp l; to_sexp r]
    | Mk (NegFilter (l, r)) -> List [Atom "≠"; to_sexp l; to_sexp r]
    | Mk (Mod (l, r)) -> List [Atom "\u{2190}"; to_sexp l; to_sexp r]
    | Mk (PosVFilter (l, r)) -> List [Atom "v="; to_sexp l; to_sexp r]
    | Mk (NegVFilter (l, r)) -> List [Atom "v≠"; to_sexp l; to_sexp r]
    | Mk (VMod (l, r)) -> List [Atom "v\u{2190}"; to_sexp l; to_sexp r]

    | Mk (Xor (l, r)) -> Sexplib0.Sexp.List [Atom "⊕"; to_sexp l; to_sexp r]
    | Mk (Diff (l, r)) -> List [Atom "-"; to_sexp l; to_sexp r]
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

    | "*" -> StarOp
    | "¬" -> NegOp
    | "forward" -> FwdOp
    | "backward" -> BwdOp

    | "∪" -> UnionOp
    | "⋅" -> SeqOp
    | "&" -> IntersectOp

    | "=" -> FilterOp true
    | "≠" -> FilterOp false
    | "\u{2190}" -> ModOp
    | "v=" -> VFilterOp true
    | "v≠" -> VFilterOp false
    | "v\u{2190}" -> VModOp

    | "⊕" -> XorOp
    | "-" -> DiffOp
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

    | XorOp, [l] -> Star (l)
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
    | _ -> 0.0
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

