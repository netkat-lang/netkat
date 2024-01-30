
type field = int
type value = int

module FieldMap = Map.Make(Int)

module ValueMap = struct
  include Map.Make(Int)
  let fold_bdgs f a m = List.fold_left (fun accum (v,b) -> f accum v b) a (bindings m)
end

module ValueSet = Set.Make(Int)

let cmp_field = Int.compare
let cmp_value = Int.compare

(* Symbol table for field names *)
let fields : (string, field) Hashtbl.t = Hashtbl.create 11
let field_labels : (field, string) Hashtbl.t = Hashtbl.create 11

let value_of_int = Fun.id
let string_of_val = string_of_int

let get_or_assign_fid (f: string) : field =
  match Hashtbl.find_opt fields f with
  | Some n -> n
  | None -> let n = Hashtbl.length fields in
            let () = Hashtbl.add fields f n in
            let () = Hashtbl.add field_labels n f in
            n

let get_or_fail_fid (n: field) : string =
  match Hashtbl.find_opt field_labels n with
  | Some f -> f
  | None -> failwith ("unknown field index: " ^ string_of_int n)

let map_op_pair op m1 m2 = ValueMap.union (fun _ x y -> Some (op x y)) m1 m2

let map_op op = List.fold_left (map_op_pair op) ValueMap.empty

let right_join (m1: 'a ValueMap.t) (m2: 'a ValueMap.t) = map_op_pair (fun a b -> b) m1 m2
let left_join (m1: 'a ValueMap.t) (m2: 'a ValueMap.t) = map_op_pair (fun a b -> a) m1 m2
(* XXX: Doesn't compile?
    left_join = Fun.flip right_join *)

let keys m =
  List.fold_left (fun s (v,_) -> ValueSet.add v s) ValueSet.empty (ValueMap.bindings m)

let union_keys ms =
  List.map keys ms |> List.fold_left ValueSet.union ValueSet.empty
