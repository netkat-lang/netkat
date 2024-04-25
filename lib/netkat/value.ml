type t = int

module M = struct
  include Map.Make(Int)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
end

module S = Set.Make(Int)

let keys m =
  List.fold_left (fun s (v,_) -> S.add v s) S.empty (M.bindings m)
let union_keys ms =
  List.map Value.keys ms |> List.fold_left Value.S.union Value.S.empty

let value_of_int = Fun.id
let string_of_val = string_of_int

let compare = Int.compare

