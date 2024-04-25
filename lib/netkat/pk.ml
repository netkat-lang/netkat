type field = Field.t
type value = Value.t

let map_op_pair d op m1 m2 = Value.M.merge(fun _ x y ->
    match x, y with
    | None, None -> None
    | None, Some y' -> Some (op d y')
    | Some x', None -> Some (op x' d)
    | Some x', Some y' -> Some (op x' y')) m1 m2

let map_op d op = List.fold_left (map_op_pair d op) Value.M.empty

let right_join d (m1: 'a Value.M.t) (m2: 'a Value.M.t) = map_op_pair d (fun a b -> b) m1 m2
let left_join d (m1: 'a Value.M.t) (m2: 'a Value.M.t) = map_op_pair d (fun a b -> a) m1 m2

(* The remaining type t and funtions deal with concrete packets. *)

type t = value Field.M.t

let val_outside (vs: Value.S.t) = Value.S.fold (fun v a -> if v = a then (max a v) + 1 else a) vs 0
