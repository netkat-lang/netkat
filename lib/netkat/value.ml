type t = int

module M = struct
  include Map.Make(Int)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
end

module S = Set.Make(Int)

let keys m =
  List.fold_left (fun s (v,_) -> S.add v s) S.empty (M.bindings m)
let union_keys ms =
  List.map keys ms |> List.fold_left S.union S.empty

let value_of_int = Fun.id
let string_of_val = string_of_int

let compare = Int.compare

let map_op_pair d op m1 m2 = M.merge(fun _ x y ->
    match x, y with
    | None, None -> None
    | None, Some y' -> Some (op d y')
    | Some x', None -> Some (op x' d)
    | Some x', Some y' -> Some (op x' y')) m1 m2

let map_op d op = List.fold_left (map_op_pair d op) M.empty

let right_join d (m1: 'a M.t) (m2: 'a M.t) = map_op_pair d (fun a b -> b) m1 m2
let left_join d (m1: 'a M.t) (m2: 'a M.t) = map_op_pair d (fun a b -> a) m1 m2

let val_outside (vs: S.t) = S.fold (fun v a -> if v = a then (max a v) + 1 else a) vs 0
